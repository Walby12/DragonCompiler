use std::collections::HashMap;
use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Clone, PartialEq)]
enum Tokens {
    Func,
    Var,
    Ident(String),
    Colon,
    ParL,
    ParR,
    CurlyL,
    CurlyR,
    Comma,
    Semicolon,
    Assign,
    Return,
    Print,
    Int(i64),
    Str(String),
    Plus,
    Import,
    If,
    Else,
    Loop,
}

fn lex(code: &str) -> Vec<Tokens> {
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
            ',' => { toks.push(Tokens::Comma); i += 1; }
            ';' => { toks.push(Tokens::Semicolon); i += 1; }
            '=' => { toks.push(Tokens::Assign); i += 1; }
            '+' => { toks.push(Tokens::Plus); i += 1; }
            ' ' | '\n' | '\t' | '\r' => { i += 1; }
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
            c if c.is_ascii_alphabetic() || c == '_' => {
                let mut builder = String::new();
                while i < chars.len() && (chars[i].is_ascii_alphanumeric() || chars[i] == '_') {
                    builder.push(chars[i]);
                    i += 1;
                }
                match builder.as_str() {
                    "func" => toks.push(Tokens::Func),
                    "var" => toks.push(Tokens::Var),
                    "return" => toks.push(Tokens::Return),
                    "print" => toks.push(Tokens::Print),
                    "import" => toks.push(Tokens::Import),
                    "if" => toks.push(Tokens::If),
                    "else" => toks.push(Tokens::Else),
                    "loop" => toks.push(Tokens::Loop),
                    _ => toks.push(Tokens::Ident(builder)),
                }
            }
            c if c.is_ascii_digit() => {
                let mut builder = String::new();
                while i < chars.len() && chars[i].is_ascii_digit() {
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

#[derive(Debug, Clone, PartialEq)]
enum Type {
    Int,
    Str,
    None,
    Unknown,
}

fn type_from_str(s: &str) -> Type {
    match s {
        "int" => Type::Int,
        "str" => Type::Str,
        "none" => Type::None,
        _ => Type::Unknown,
    }
}

fn rust_type(ty: &Type) -> &'static str {
    match ty {
        Type::Int => "i64",
        Type::Str => "String",
        Type::None => "()",
        Type::Unknown => "()",
    }
}

#[derive(Debug, Clone)]
struct FuncSig {
    params: Vec<Type>,
    ret: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Stmt {
    VarDecl { name: String, var_type: String, value: Expr },
    Assign { name: String, value: Expr },
    Print { expr: Expr },
    Return { expr: Option<Expr> },
    ExprStmt { expr: Expr },
    Import { file: String },
    If {
        condition: Expr,
        then_block: Vec<Stmt>,
        else_block: Option<Vec<Stmt>>,
    },
    Loop {
        condition: Expr,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Expr {
    Int(i64),
    Str(String),
    Ident(String),
    Call { name: String, args: Vec<Expr> },
    Add(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone)]
struct Function {
    name: String,
    return_type: String,
    params: Vec<(String, String)>,
    body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
struct Program {
    funcs: Vec<Function>,
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
    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while let Some(tok) = self.peek() {
            if matches!(tok, Tokens::CurlyR) {
                break;
            }
            let stmt = self.parse_stmt("none")?;
            stmts.push(stmt);
        }
        Ok(stmts)
    }
    fn load_import(&mut self, file: &str, funcs: &mut Vec<Function>) -> Result<(), ParseError> {
        let path = format!("{}.drc", file);
        let code = std::fs::read_to_string(&path)
            .map_err(|_| ParseError { message: format!("Import file not found: {}", path), position: self.pos })?;
        let tokens = lex(&code);
        let mut parser = Parser::new(tokens);
        let mut imported_prog = parser.parse_program()?;

        funcs.extend(imported_prog.funcs);

        Ok(())
    }


    fn new(tokens: Vec<Tokens>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Tokens> {
        self.tokens.get(self.pos)
    }

    fn peek_n(&self, n: usize) -> Option<&Tokens> {
        self.tokens.get(self.pos + n)
    }

    fn advance(&mut self) -> Option<Tokens> {
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: Tokens) -> Result<(), ParseError> {
        let pos = self.pos;
        let got = self.advance().ok_or(ParseError {
            message: format!("Parse error at token {}: expected {:?}, but reached end", pos, expected),
            position: pos,
        })?;
        if std::mem::discriminant(&got) != std::mem::discriminant(&expected) {
            return Err(ParseError {
                message: format!("Parse error at token {}: expected {:?}, found {:?}", pos, expected, got),
                position: pos,
            });
        }
        Ok(())
    }

    fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut funcs = Vec::new();

        while let Some(token) = self.peek() {
            match token {
                Tokens::Func => funcs.push(self.parse_function()?),
                Tokens::Import => { 
                    let stmt = self.parse_stmt("none")?; 
                    if let Stmt::Import { file } = stmt {
                        self.load_import(&file, &mut funcs)?;
                    }
                }
                _ => return Err(ParseError { 
                    message: format!("Unexpected token at top-level: {:?}", token), 
                    position: self.pos 
                }),
            }
        }

        Ok(Program { funcs })
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        self.expect(Tokens::Func)?;

        let name = match self.advance() {
            Some(Tokens::Ident(n)) => n,
            other => {
                return Err(ParseError {
                    message: format!("Parse error at token {}: expected function name, found {:?}", self.pos - 1, other),
                    position: self.pos - 1,
                })
            }
        };

        self.expect(Tokens::Colon)?;
        let return_type = match self.advance() {
            Some(Tokens::Ident(rt)) => rt,
            other => {
                return Err(ParseError {
                    message: format!("Parse error at token {}: expected return type after `:` in function `{}`: found {:?}", self.pos-1, name, other),
                    position: self.pos - 1,
                })
            }
        };

        self.expect(Tokens::ParL)?;
        let mut params = Vec::new();
        if !matches!(self.peek(), Some(Tokens::ParR)) {
            loop {
                let pname = match self.advance() {
                    Some(Tokens::Ident(n)) => n,
                    other => {
                        return Err(ParseError {
                            message: format!("Expected parameter name, found {:?}", other),
                            position: self.pos - 1,
                        })
                    }
                };
                self.expect(Tokens::Colon)?;
                let ptype = match self.advance() {
                    Some(Tokens::Ident(t)) => t,
                    other => {
                        return Err(ParseError {
                            message: format!("Expected parameter type, found {:?}", other),
                            position: self.pos - 1,
                        })
                    }
                };
                params.push((pname, ptype));
                if matches!(self.peek(), Some(Tokens::Comma)) {
                    self.advance();
                    continue;
                } else {
                    break;
                }
            }
        }
        self.expect(Tokens::ParR)?;
        self.expect(Tokens::CurlyL)?;

        let mut body = Vec::new();
        while !matches!(self.peek(), Some(Tokens::CurlyR)) {
            body.push(self.parse_stmt(&return_type)?);
        }

        self.expect(Tokens::CurlyR)?;

        Ok(Function { name, return_type, params, body })
    }

    fn parse_stmt(&mut self, current_return_type: &str) -> Result<Stmt, ParseError> {
        match self.peek() {
            Some(Tokens::If) => self.parse_if(),
            Some(Tokens::Loop) => self.parse_loop(),
            Some(Tokens::Return) => {
                self.advance();
                if matches!(self.peek(), Some(Tokens::Semicolon)) {
                    self.advance();
                    if current_return_type != "none" {
                        return Err(ParseError {
                            message: format!("Semantic error: return without value in function returning `{}`", current_return_type),
                            position: self.pos,
                        });
                    }
                    Ok(Stmt::Return { expr: None })
                } else {
                    let e = self.parse_expr()?;
                    self.expect(Tokens::Semicolon)?;
                    if current_return_type == "none" {
                        return Err(ParseError {
                            message: "Semantic error: return with value in function returning `none`".into(),
                            position: self.pos,
                        });
                    }
                    Ok(Stmt::Return { expr: Some(e) })
                }
            }
            Some(Tokens::Var) => {
                self.advance();
                let name = match self.advance() {
                    Some(Tokens::Ident(n)) => n,
                    other => {
                        return Err(ParseError {
                            message: format!("Expected identifier after `var`, found {:?}", other),
                            position: self.pos - 1,
                        })
                    }
                };
                self.expect(Tokens::Colon)?;
                let var_type = match self.advance() {
                    Some(Tokens::Ident(t)) => t,
                    other => {
                        return Err(ParseError {
                            message: format!("Expected type after `:` in var declaration, found {:?}", other),
                            position: self.pos - 1,
                        })
                    }
                };
                self.expect(Tokens::Assign)?;
                let value = self.parse_expr()?;
                self.expect(Tokens::Semicolon)?;
                Ok(Stmt::VarDecl { name, var_type, value })
            }
            Some(Tokens::Print) => {
                self.advance();
                let expr = self.parse_expr()?;
                self.expect(Tokens::Semicolon)?;
                Ok(Stmt::Print { expr })
            }
            Some(Tokens::Ident(_)) => {
                let e = self.parse_expr()?;
                self.expect(Tokens::Semicolon)?;
                Ok(Stmt::ExprStmt { expr: e })
            }
            Some(Tokens::Import) => {
                self.advance();
                let name = match self.advance() {
                    Some(Tokens::Str(s)) => s,
                    Some(Tokens::Ident(s)) => s,
                    other => {
                        return Err(ParseError {
                            message: format!("Expected string or identifier after import, found {:?}", other),
                            position: self.pos - 1,
                        })
                    }
                };
                self.expect(Tokens::Semicolon)?;
                Ok(Stmt::Import { file: name })
            }
            other => Err(ParseError {
                message: format!("Unexpected token: {:?}", other),
                position: self.pos,
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = match self.advance() {
            Some(Tokens::Int(n)) => Expr::Int(n),
            Some(Tokens::Str(s)) => Expr::Str(s),
            Some(Tokens::Ident(s)) => {
                if matches!(self.peek(), Some(Tokens::ParL)) {
                    self.advance(); // consume '('
                    let mut args = Vec::new();
                    if !matches!(self.peek(), Some(Tokens::ParR)) {
                        loop {
                            args.push(self.parse_expr()?);
                            if matches!(self.peek(), Some(Tokens::Comma)) {
                                self.advance();
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(Tokens::ParR)?;
                    Expr::Call { name: s, args }
                } else {
                    Expr::Ident(s)
                }
            }
            Some(Tokens::ParL) => {
                let expr = self.parse_expr()?;
                self.expect(Tokens::ParR)?;
                expr
            }
            other => return Err(ParseError {
                message: format!("Unexpected token in expression: {:?}", other),
                position: self.pos,
            }),
        };

        while matches!(self.peek(), Some(Tokens::Plus)) {
            self.advance();
            let right = self.parse_expr()?;
            left = Expr::Add(Box::new(left), Box::new(right));
        }

        Ok(left)
    }
    fn parse_if(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Tokens::If)?;
        self.expect(Tokens::ParL)?;
        let cond = self.parse_expr()?; 
        self.expect(Tokens::ParR)?;
        self.expect(Tokens::CurlyL)?;
        let then_block = self.parse_block()?;
        self.expect(Tokens::CurlyR)?;

        let else_block = if matches!(self.peek(), Some(Tokens::Else)) {
            self.advance();
            self.expect(Tokens::CurlyL)?;
            let block = self.parse_block()?;
            self.expect(Tokens::CurlyR)?;
            Some(block)
        } else {
            None
        };

        Ok(Stmt::If { condition: cond, then_block, else_block })
    }
    fn parse_loop(&mut self) -> Result<Stmt, ParseError> {
        self.expect(Tokens::Loop)?;
        self.expect(Tokens::ParL)?;
        let cond = self.parse_expr()?;
        self.expect(Tokens::ParR)?;
        self.expect(Tokens::CurlyL)?;
        let body = self.parse_block()?;
        self.expect(Tokens::CurlyR)?;
        Ok(Stmt::Loop { condition: cond, body })
    }
}

impl Program {
    fn resolve_imports(&mut self, base_path: &Path) -> Result<(), String> {
        let mut new_funcs = Vec::new();
        let mut to_remove = Vec::new();

        for (i, func) in self.funcs.iter().enumerate() {
            for stmt in &func.body {
                if let Stmt::Import { file } = stmt {
                    let import_path = base_path.join(format!("{}.drc", file));
                    if !import_path.exists() {
                        return Err(format!("Import file `{}` not found", import_path.display()));
                    }

                    let code = fs::read_to_string(&import_path)
                        .map_err(|e| format!("Failed to read {}: {}", import_path.display(), e))?;
                    let tokens = lex(&code);
                    let mut parser = Parser::new(tokens);
                    let mut imported_prog = parser.parse_program()
                        .map_err(|e| format!("Parse error in imported file {}: {}", import_path.display(), e.message))?;

                    imported_prog.resolve_imports(import_path.parent().unwrap())?;

                    new_funcs.extend(imported_prog.funcs);

                    to_remove.push((i, stmt.clone()));
                }
            }
        }

        for (i, stmt) in to_remove.into_iter().rev() {
            if let Some(f) = self.funcs.get_mut(i) {
                f.body.retain(|s| s != &stmt);
            }
        }

        self.funcs.extend(new_funcs);
        Ok(())
    }

    fn require_main(&self) -> Result<(), String> {
        if !self.funcs.iter().any(|f| f.name == "main") {
            return Err("Program must have a main function".into());
        }
        Ok(())
    }

    fn check_types(&self) -> Result<(), String> {
        for f in &self.funcs {
            let expected_ret = type_from_str(&f.return_type);
            let mut has_return = false;
            for stmt in &f.body {
                match stmt {
                    Stmt::Return { expr } => {
                        has_return = true;
                        if let Some(e) = expr {
                            let actual = match e {
                                Expr::Int(_) => Type::Int,
                                Expr::Str(_) => Type::Str,
                                Expr::Ident(_) => Type::Unknown,
                                _ => Type::Unknown,
                            };
                            if actual != expected_ret && actual != Type::Unknown {
                                return Err(format!("Type error in function {}: expected {:?}, got {:?}", f.name, expected_ret, actual));
                            }
                        } else if expected_ret != Type::None {
                            return Err(format!("Type error: function {} should return {:?}, but returns nothing", f.name, expected_ret));
                        }
                    }
                    Stmt::Import { .. } => {}
                    _ => {}
                }
            }

            if expected_ret != Type::None && !has_return {
                return Err(format!("Type error: function `{}` with return type `{:?}` is missing a return statement", f.name, expected_ret));
            }
        }
        Ok(())
    }
}

fn gen_expr(e: &Expr) -> String {
    match e {
        Expr::Int(n) => n.to_string(),
        Expr::Str(s) => format!("String::from({:?})", s),
        Expr::Ident(s) => s.clone(),
        Expr::Call { name, args } => {
            let args_str = args.iter().map(gen_expr).collect::<Vec<_>>().join(", ");
            format!("{}({})", name, args_str)
        }
        Expr::Add(l, r) => format!("({} + {})", gen_expr(l), gen_expr(r)),
    }
}

fn gen_stmt(s: &Stmt) -> String {
    match s {
        Stmt::If { condition, then_block, else_block } => {
            let mut code = String::new();
            code.push_str("if ");
            code.push_str(&gen_expr(condition)); // FIXED
            code.push_str(" {\n");
            for stmt in then_block {
                code.push_str(&gen_stmt(stmt)); // FIXED
            }
            code.push_str("}\n");
            if let Some(else_blk) = else_block {
                code.push_str("else {\n");
                for stmt in else_blk {
                    code.push_str(&gen_stmt(stmt)); // FIXED
                }
                code.push_str("}\n");
            }
            code
        }
        Stmt::Loop { condition, body } => {
            let mut code = String::new();
            code.push_str("while ");
            code.push_str(&gen_expr(condition)); // FIXED
            code.push_str(" {\n");
            for stmt in body {
                code.push_str(&gen_stmt(stmt)); // FIXED
            }
            code.push_str("}\n");
            code
        }
        Stmt::VarDecl { name, var_type, value } => {
            let ty = type_from_str(var_type);
            format!("let mut {}: {} = {};", name, rust_type(&ty), gen_expr(value))
        }
        Stmt::Assign { name, value } => format!("{} = {};", name, gen_expr(value)),
        Stmt::Print { expr } => format!("println!(\"{{}}\", {});", gen_expr(expr)),
        Stmt::Return { expr } => match expr {
            None => "return;".to_string(),
            Some(e) => format!("return {};", gen_expr(e)),
        },
        Stmt::ExprStmt { expr } => format!("{};", gen_expr(expr)),
        Stmt::Import { .. } => "".into(),
    }
}

fn generate_rust(prog: &Program) -> String {
    let mut out = String::new();
    for f in &prog.funcs {
        if f.name == "main" {
            out.push_str("fn main() {\n");
            for stmt in &f.body {
                out.push_str("    ");
                out.push_str(&gen_stmt(stmt));
                out.push('\n');
            }
            out.push_str("}\n\n");
        } else {
            let sig_params = f.params.iter()
                .map(|(n,t)| format!("{}: {}", n, rust_type(&type_from_str(t))))
                .collect::<Vec<_>>()
                .join(", ");
            let ret = rust_type(&type_from_str(&f.return_type));
            out.push_str(&format!("fn {}({}) -> {} {{\n", f.name, sig_params, ret));
            for stmt in &f.body {
                out.push_str("    ");
                out.push_str(&gen_stmt(stmt));
                out.push('\n');
            }
            out.push_str("}\n\n");
        }
    }
    out
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
        eprintln!("Usage: {} <input.drc> [output.rs]", args[0]);
        return;
    }

    let file_name = &args[1];
    let path = Path::new(file_name);
    if path.extension().and_then(|s| s.to_str()) != Some("drc") {
        eprintln!("ERROR: Input file must have .drc extension");
        return;
    }

    let out_file_name = if args.len() >= 3 {
        args[2].clone()
    } else {
        let stem = path.file_stem().unwrap().to_str().unwrap();
        path.with_file_name(format!("{stem}.rs")).to_string_lossy().to_string()
    };

    let code = fs::read_to_string(file_name).expect("Could not read input file");
    let tokens = lex(&code);
    let mut parser = Parser::new(tokens);

    let mut prog = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse failed: {} at token {}", e.message, e.position);
            std::process::exit(1);
        }
    };

    let base_path = path.parent().unwrap_or(Path::new("."));
    if let Err(e) = prog.resolve_imports(base_path) {
        eprintln!("Import error: {}", e);
        std::process::exit(1);
    }

    if let Err(e) = prog.require_main() {
        eprintln!("{}", e);
        std::process::exit(1);
    }

    if let Err(e) = prog.check_types() {
        eprintln!("Type check failed: {}", e);
        std::process::exit(1);
    }

    let rust_code = generate_rust(&prog);
    let mut file = File::create(&out_file_name).expect("Could not create output file");
    file.write_all(rust_code.as_bytes()).expect("Could not write output file");

    let exe_name = Path::new(&out_file_name).with_extension("").to_string_lossy().to_string();
    compile_with_rustc(&out_file_name, &exe_name);

    println!("Compilation successful: {}", exe_name);
}
