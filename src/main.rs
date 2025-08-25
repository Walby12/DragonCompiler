use std::fs::File;
use std::io::Write;
use std::env;
use std::path::Path;
use std::fs;
use std::process::Command;

#[derive(Debug)]
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
    Semicolon,
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
}

fn parse(n_parsed: String) -> Vec<Tokens> {
    let mut toks: Vec<Tokens> = Vec::new();
    let chars: Vec<char> = n_parsed.chars().collect();

    let mut i = 0;
    
    while i != chars.len() {
        let c = chars[i];

        match c {
            ':' => {
                toks.push(Tokens::Colon);
                i += 1;
            }
            '(' => {
                toks.push(Tokens::ParL);
                i += 1;
            }
            ')' => {
                toks.push(Tokens::ParR);
                i += 1;
            }
            '{' => {
                toks.push(Tokens::CurlyL);
                i += 1;
            }
            '}' => {
                toks.push(Tokens::CurlyR);
                i += 1;
            }
            ';' => {
                toks.push(Tokens::Semicolon);
                i += 1;
            }
            ' ' | '\n' | '\t' => {
                i += 1;
            }
            mut c if c.is_alphabetic() => {
                let mut builder: String = String::new();

                while c.is_alphabetic() {
                    builder += &c.to_string();
                    i += 1;
                    c = chars[i];
                }

                if builder == "func" {
                    toks.push(Tokens::Func);
                } else if builder == "return" {
                    toks.push(Tokens::Return);
                } else {
                    toks.push(Tokens::Ident(builder));
                }
            }
            mut c if c.is_numeric() => {
                let mut builder: String = String::new();

                while c.is_numeric() {
                    builder += &c.to_string();
                    i += 1;
                    c = chars[i];
                }
                toks.push(Tokens::Int(builder.parse::<i64>().expect("ERROR: Could not parse i64")))
            }
            _ => {
                panic!("ERROR: unparsable token: {:?}", c);
            }
        }
    }
    toks
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

    fn expect(&mut self, expected: Tokens) {
        let tok = self.advance().expect("ERROR: Unexpected EOF");
        if std::mem::discriminant(tok) != std::mem::discriminant(&expected) {
            panic!("ERROR: Expected {:?}, got {:?}", expected, tok);
        }
    }
}

impl Parser {
    fn parse_program(&mut self) -> ASTNode {
        let mut funcs = Vec::new();
        while self.peek().is_some() {
            funcs.push(self.parse_function());
        }
        ASTNode::Program(funcs)
    }

    fn parse_function(&mut self) -> ASTNode {
        self.expect(Tokens::Func);
        
        let name = if let Tokens::Ident(n) = self.advance().unwrap() {
            n.clone()
        } else {
            panic!("ERROR: Expected function name");
        };

        self.expect(Tokens::Colon);
        let return_type = if let Tokens::Ident(rt) = self.advance().unwrap() {
            rt.clone()
        } else {
            panic!("ERROR: Expected return type");
        };

        self.expect(Tokens::ParL);
        self.expect(Tokens::ParR);

        self.expect(Tokens::CurlyL);
        let mut body = Vec::new();
        while !matches!(self.peek(), Some(Tokens::CurlyR)) {
            body.push(self.parse_stmt());
        }
        self.expect(Tokens::CurlyR);

        ASTNode::Function {
            name,
            return_type,
            params: Vec::new(),
            body,
        }
    }

    fn parse_stmt(&mut self, current_return_type: &str) -> ASTNode {
        match self.peek() {
            Some(Tokens::Return) => {
                self.advance();
                if matches!(self.peek(), Some(Tokens::Semicolon)) {
                    self.expect(Tokens::Semicolon);
                    if current_return_type != "none" {
                        panic!("ERROR: return without value in function returning {current_return_type}");
                    }
                    ASTNode::Return(Box::new(ASTNode::Ident("".into())))
                } else {
                    let expr = self.parse_expr();
                    self.expect(Tokens::Semicolon);
                    if current_return_type == "none" {
                        panic!("ERROR: return with value in function returning none");
                    }
                    ASTNode::Return(Box::new(expr))
                }
            }
            _ => panic!("Unexpected token in statement: {:?}", self.peek()),
        }
    }


    fn parse_expr(&mut self) -> ASTNode {
        match self.advance() {
            Some(Tokens::Int(n)) => ASTNode::IntLiteral(*n),
            Some(Tokens::Ident(s)) => ASTNode::Ident(s.clone()),
            other => panic!("ERROR: Unexpected token in expression: {:?}", other),
        }
    }
}

impl ASTNode {
    fn require_main(&self) {
        if let ASTNode::Program(funcs) = self {
            for func in funcs {
                if let ASTNode::Function { name, .. } = func {
                    if name == "main" {
                        return;
                    }
                }
            }
            panic!("ERROR: Program must have a `main` function");
        } else {
            panic!("ERROR: Top-level node must be a Program");
        }
    }
}

fn generate_code(ast: &ASTNode) -> String {
    match ast {
        ASTNode::Program(funcs) => {
            let mut code = String::new();
            for func in funcs {
                code.push_str(&generate_code(func));
                code.push('\n');
            }
            code
        }
        ASTNode::Function { name, return_type, params, body } => {
            let params_str: Vec<String> = params
                .iter()
                .map(|(pname, ptype)| format!("{pname}: {ptype}"))
                .collect();

            let mut code = String::new();

            if name == "main" {
                if return_type == "none" {
                    code.push_str(&format!("fn main() {{\n"));
                    for stmt in body {
                        code.push_str("    ");
                        code.push_str(&generate_code(stmt));
                        code.push('\n');
                    }
                } else if return_type == "int" {
                    code.push_str(&format!("fn main() {{\n"));
                    for stmt in body {
                        match stmt {
                            ASTNode::Return(expr) => {
                                code.push_str("    std::process::exit(");
                                code.push_str(&generate_code(expr));
                                code.push_str(");\n");
                            }
                            other => {
                                code.push_str("    ");
                                code.push_str(&generate_code(other));
                                code.push('\n');
                            }
                        }
                    }
                } else {
                    panic!("ERROR: main must return either 'none' or 'int'");
                }
            } else {
                let rust_type = match return_type.as_str() {
                    "int" => "i64",
                    "none" => "()",
                    _ => panic!("Unknown return type: {}", return_type),
                };
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


        ASTNode::Return(expr) => {
            format!("return {};", generate_code(expr))
        }
        ASTNode::IntLiteral(n) => {
            n.to_string()
        }
        ASTNode::Ident(s) => {
            s.clone()
        }
    }
}

fn compile_with_rustc(rs_file: &str, out_file: &str) {
    let status = Command::new("rustc")
        .arg(rs_file)
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

    if args.len() <= 1 {
        eprintln!("ERROR: Not enough args for compilation\n\nUsage: {} <file_name> [out_file_name]", args[0]);
        return;
    }

    let file_name = &args[1];

    let out_file_name = if args.len() == 3 {
        args[2].clone()
    } else {
        let path = Path::new(file_name);

        if path.extension().and_then(|ext| ext.to_str()) != Some("drc") {
            eprintln!("ERROR: Input file must have .drc extension");
            return;
        }

        let stem = path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or(file_name);
        let mut out_path = path.with_file_name(format!("{stem}.rs"));
        out_path.to_string_lossy().to_string()
    };


    let code = fs::read_to_string(file_name).expect("ERROR: Could not read input file");

    let tokens = parse(code);
    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program();
    ast.require_main();

    let code = generate_code(&ast);

    let mut file = File::create(&out_file_name).expect("ERROR: Could not create output file"); 
    file.write_all(code.as_bytes()).expect("ERROR: Could not write to output file");

    let exe_name = Path::new(&out_file_name)
        .with_extension("")
        .to_string_lossy()
        .to_string();

    compile_with_rustc(&out_file_name, &exe_name);

    println!("Compiled executable: {exe_name}");

}
