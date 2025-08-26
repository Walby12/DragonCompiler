use std::fs::{self, File};
use std::io::Write;
use std::env;
use std::path::Path;
use std::process::Command;

#[derive(Debug, Clone)]
enum Tokens {
    Func,
    Ident(String),
    Colon,
    ParL,
    ParR,
    CurlyL,
    CurlyR,
    Return,
    Int(i64),
    Str(String),
    Semicolon,
    Print,
}

#[derive(Debug)]
enum ASTNode {
    Program(Vec<ASTNode>),
    Function {
        name: String,
        return_type: String,
        params: Vec<(String, String)>,
        body: Vec<ASTNode>,
    },
    Return(Box<ASTNode>),
    IntLiteral(i64),
    Ident(String),
    Print {
        thing: String,
        print_t: String,
    },
}

#[derive(Debug)]
struct ParseError {
    message: String,
    position: usize,
}

struct Parser {
    tokens: Vec<Tokens>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Tokens>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Tokens> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<&Tokens> {
        let tok = self.tokens.get(self.pos);
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: Tokens) -> Result<(), ParseError> {
        let pos = self.pos;
        let tok = self.advance().ok_or(ParseError {
            message: format!(
                "Parse error at token {}: expected {:?}, but reached end of file",
                pos, expected
            ),
            position: pos,
        })?;

        if std::mem::discriminant(tok) != std::mem::discriminant(&expected) {
            return Err(ParseError {
                message: format!(
                    "Parse error at token {}: expected {:?}, found {:?}",
                    pos, expected, tok
                ),
                position: pos,
            });
        }

        Ok(())
    }

    fn parse_program(&mut self) -> Result<ASTNode, ParseError> {
        let mut funcs = Vec::new();
        while self.peek().is_some() {
            funcs.push(self.parse_function()?);
        }
        Ok(ASTNode::Program(funcs))
    }

    fn parse_function(&mut self) -> Result<ASTNode, ParseError> {
        self.expect(Tokens::Func)?;

        let name = match self.advance() {
            Some(Tokens::Ident(n)) => n.clone(),
            _ => {
                return Err(ParseError {
                    message: format!(
                        "Parse error at token {}: expected function name after `func`",
                        self.pos
                    ),
                    position: self.pos,
                })
            }
        };

        self.expect(Tokens::Colon)?;
        let return_type = match self.advance() {
            Some(Tokens::Ident(rt)) => rt.clone(),
            _ => {
                return Err(ParseError {
                    message: format!(
                        "Parse error at token {}: expected return type after `:` in function `{}`",
                        self.pos, name
                    ),
                    position: self.pos,
                })
            }
        };

        self.expect(Tokens::ParL)?;
        self.expect(Tokens::ParR)?;
        self.expect(Tokens::CurlyL)?;

        let mut body = Vec::new();
        while !matches!(self.peek(), Some(Tokens::CurlyR)) {
            body.push(self.parse_stmt(&return_type)?);
        }

        self.expect(Tokens::CurlyR)?;

        Ok(ASTNode::Function {
            name,
            return_type,
            params: Vec::new(),
            body,
        })
    }
    fn parse_print(&mut self) -> Result<ASTNode, ParseError> {
        self.expect(Tokens::Print)?;

        self.expect(Tokens::ParL)?;

        let mut print_t = String::new();

        let thing = match self.advance() {
            Some(Tokens::Str(n)) => {
                print_t = String::from("str");
                n.clone()
            }
            Some(Tokens::Int(n)) => {
                print_t = String::from("int");
                n.to_string()
            }
            _ => {
                return Err(ParseError {
                    message: format!(
                        "Parse error at token {}: expected a string name after `print`",
                        self.pos
                    ),
                    position: self.pos,
                })
            }
        };

        self.expect(Tokens::ParR)?;
        self.expect(Tokens::Semicolon)?;

        Ok(ASTNode::Print {
            thing,
            print_t,
        })

    }

    fn parse_stmt(&mut self, current_return_type: &str) -> Result<ASTNode, ParseError> {
        match self.peek() {
            Some(Tokens::Return) => {
                self.advance();
                let expr = if matches!(self.peek(), Some(Tokens::Semicolon)) {
                    self.expect(Tokens::Semicolon)?;
                    if current_return_type != "none" {
                        return Err(ParseError {
                            message: format!(
                                "Semantic error: return without value in function returning `{}`",
                                current_return_type
                            ),
                            position: self.pos,
                        });
                    }
                    Box::new(ASTNode::Ident("".into()))
                } else {
                    let e = self.parse_expr()?;
                    self.expect(Tokens::Semicolon)?;
                    if current_return_type == "none" {
                        return Err(ParseError {
                            message: "Semantic error: return with value in function returning `none`".into(),
                            position: self.pos,
                        });
                    }
                    Box::new(e)
                };
                Ok(ASTNode::Return(expr))
            }
            Some(Tokens::Print) => self.parse_print(),
            other => Err(ParseError {
                message: format!("Unexpected token in statement: {:?}", other),
                position: self.pos,
            }),
        }
    }


    fn parse_expr(&mut self) -> Result<ASTNode, ParseError> {
        match self.advance() {
            Some(Tokens::Int(n)) => Ok(ASTNode::IntLiteral(*n)),
            Some(Tokens::Ident(s)) => Ok(ASTNode::Ident(s.clone())),
            other => Err(ParseError {
                message: format!("Unexpected token in expression: {:?}", other),
                position: self.pos,
            }),
        }
    }
}

impl ASTNode {
    fn require_main(&self) -> Result<(), String> {
        if let ASTNode::Program(funcs) = self {
            for func in funcs {
                if let ASTNode::Function { name, return_type, .. } = func {
                    if name == "main" {
                        if return_type != "none" {
                            return Err(format!(
                                "Program error: `main` function must have return type `none`, found `{}`",
                                return_type
                            ));
                        }
                        return Ok(());
                    }
                }
            }
            Err("Program error: no `main` function found. Every program must have a `main` function.".into())
        } else {
            Err("Internal error: top-level AST node is not a Program.".into())
        }
    }
}

fn generate_code(ast: &ASTNode) -> String {
    match ast {
        ASTNode::Program(funcs) => funcs.iter().map(generate_code).collect::<Vec<_>>().join("\n"),
        ASTNode::Function { name, return_type, params, body } => {
            let params_str: Vec<String> = params.iter().map(|(n, t)| format!("{n}: {t}")).collect();
            let mut code = String::new();
            let rust_type = match return_type.as_str() {
                "int" => "i64",
                "none" => "()",
                other => panic!("Unknown return type `{}`", other),
            };
            if name == "main" {
                code.push_str("fn main() {\n");
                for stmt in body {
                    code.push_str("    ");
                    code.push_str(&generate_code(stmt));
                    code.push('\n');
                }
            } else {
                code.push_str(&format!("fn {name}({}) -> {rust_type} {{\n", params_str.join(", ")));
                for stmt in body {
                    code.push_str("    ");
                    code.push_str(&generate_code(stmt));
                    code.push('\n');
                }
            }
            code.push_str("}\n");
            code
        }
        ASTNode::Print { thing, print_t } => {
            let mut code = String::new();
    
            if print_t == "str" {
                code.push_str(&format!("println!(\"{}\");", thing));
            } else {
                code.push_str(&format!("println!(\"{}\");", thing));
            }

            code
        }
        ASTNode::Return(expr) => {
            if let ASTNode::Ident(ref s) = **expr {
                if s.is_empty() {
                    "return;".into()
                } else {
                    format!("return {};", generate_code(expr))
                }
            } else {
                format!("return {};", generate_code(expr))
            }
        }
        ASTNode::IntLiteral(n) => n.to_string(),
        ASTNode::Ident(s) => s.clone(),
    }
}

fn parse(code: String) -> Vec<Tokens> {
    let mut toks: Vec<Tokens> = Vec::new();
    let chars: Vec<char> = code.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];
        match c {
            ':' => { toks.push(Tokens::Colon); i += 1; }
            '(' => { toks.push(Tokens::ParL); i += 1; }
            ')' => { toks.push(Tokens::ParR); i += 1; }
            '{' => { toks.push(Tokens::CurlyL); i += 1; }
            '}' => { toks.push(Tokens::CurlyR); i += 1; }
            ';' => { toks.push(Tokens::Semicolon); i += 1; }
            ' ' | '\n' | '\t' => { i += 1; }
            '"' => {
                i += 1;
                let mut builder = String::new();
                while i < chars.len() && chars[i] != '"' {
                    builder.push(chars[i]);
                    i += 1;
                }
                if i >= chars.len() {
                    panic!("Unterminated string literal");
                }
                i += 1;
                toks.push(Tokens::Str(builder));
            }

            c if c.is_alphabetic() => {
                let mut builder = String::new();
                while i < chars.len() && chars[i].is_alphabetic() {
                    builder.push(chars[i]);
                    i += 1;
                }
                match builder.as_str() {
                    "func" => toks.push(Tokens::Func),
                    "return" => toks.push(Tokens::Return),
                    "print" => toks.push(Tokens::Print),
                    _ => toks.push(Tokens::Ident(builder)),
                }
            }
            c if c.is_numeric() => {
                let mut builder = String::new();
                while i < chars.len() && chars[i].is_numeric() {
                    builder.push(chars[i]);
                    i += 1;
                }
                toks.push(Tokens::Int(builder.parse::<i64>().unwrap()));
            }
            _ => panic!("Unrecognized token: {}", c),
        }
    }

    toks
}

fn compile_with_rustc(rs_file: &str, out_file: &str) {
    let status = Command::new("rustc")
        .arg(rs_file)
        .arg("-Awarnings")
        .arg("-C")
        .arg("opt-level=3")
        .arg("-o")
        .arg(out_file)
        .status()
        .expect("Failed to run rustc");

    if !status.success() {
        eprintln!("rustc failed with status: {}", status);
        std::process::exit(1);
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input.drc> [output]", args[0]);
        return;
    }

    let file_name = &args[1];
    let path = Path::new(file_name);

    if path.extension().and_then(|s| s.to_str()) != Some("drc") {
        eprintln!("ERROR: Input file must have .drc extension");
        return;
    }

    let out_file_name = if args.len() == 3 {
        args[2].clone()
    } else {
        let stem = path.file_stem().unwrap().to_str().unwrap();
        let mut out_path = path.with_file_name(format!("{stem}.rs"));
        out_path.to_string_lossy().to_string()
    };

    let code = fs::read_to_string(file_name).expect("Could not read input file");
    let tokens = parse(code);
    let mut parser = Parser::new(tokens);

    let ast = parser.parse_program().expect("Parse failed");
    ast.require_main().expect("Program must have a main function");

    let rust_code = generate_code(&ast);

    let mut file = File::create(&out_file_name).expect("Could not create output file");
    file.write_all(rust_code.as_bytes()).expect("Could not write output file");

    let exe_name = Path::new(&out_file_name).with_extension("").to_string_lossy().to_string();
    compile_with_rustc(&out_file_name, &exe_name);

    println!("Compilation succesful: {}", exe_name);
}
