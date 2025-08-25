use std::fs::File;
use std::io::Write;

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
                toks.push(Tokens::Int(builder.parse::<i64>().expect("Could not parse i64")))
            }
            _ => {
                eprintln!("ERROR: unparsable token: {:?}", c);
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
        let tok = self.advance().expect("Unexpected EOF");
        if std::mem::discriminant(tok) != std::mem::discriminant(&expected) {
            panic!("Expected {:?}, got {:?}", expected, tok);
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
            panic!("Expected function name");
        };

        self.expect(Tokens::Colon);
        let return_type = if let Tokens::Ident(rt) = self.advance().unwrap() {
            rt.clone()
        } else {
            panic!("Expected return type");
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

    fn parse_stmt(&mut self) -> ASTNode {
        match self.peek() {
            Some(Tokens::Return) => {
                self.advance();
                let expr = self.parse_expr();
                self.expect(Tokens::Semicolon);
                ASTNode::Return(Box::new(expr))
            }
            _ => panic!("Unexpected token in statement: {:?}", self.peek()),
        }
    }

    fn parse_expr(&mut self) -> ASTNode {
        match self.advance() {
            Some(Tokens::Int(n)) => ASTNode::IntLiteral(*n),
            Some(Tokens::Ident(s)) => ASTNode::Ident(s.clone()),
            other => panic!("Unexpected token in expression: {:?}", other),
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
            panic!("Error: Program must have a `main` function");
        } else {
            panic!("Top-level node must be a Program");
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
            let mut ret_type: String = String::new();
            if return_type == "int" {
                ret_type = String::from("i64");
            } else {
                eprintln!("ERROR: Unexpected return type: {}", return_type);
            }
            let params_str: Vec<String> = params
                .iter()
                .map(|(pname, ptype)| format!("{pname}: {ptype}"))
                .collect();

            let mut code = format!("fn {name}({}) -> {ret_type} {{\n", params_str.join(", "));

            for stmt in body {
                code.push_str("    ");
                code.push_str(&generate_code(stmt));
                code.push('\n');
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

fn main() {
    let code = String::from("func main: int () { return 12; }");
    let tokens = parse(code);

    let mut parser = Parser::new(tokens);
    let ast = parser.parse_program();
    ast.require_main();

    let code = generate_code(&ast);

    let mut file = File::create("test.rs").expect("Could not crate file"); 
    
    file.write_all(code.as_bytes()).expect("Could not write to file");
}
