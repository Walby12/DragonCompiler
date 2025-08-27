use std::collections::HashMap;
use std::env;
use std::fs::{self, File};
use std::io::Write;
use std::path::Path;
use std::process::Command;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
enum Stmt {
    VarDecl { name: String, var_type: String, value: Expr },
    Assign { name: String, value: Expr },
    Print { expr: Expr },
    Return { expr: Option<Expr> },
    ExprStmt { expr: Expr },
}

#[derive(Debug, Clone)]
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
        while self.peek().is_some() {
            funcs.push(self.parse_function()?);
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
                self.expect(Tokens::ParL)?;
                let expr = self.parse_expr()?;
                self.expect(Tokens::ParR)?;
                self.expect(Tokens::Semicolon)?;
                Ok(Stmt::Print { expr })
            }
            Some(Tokens::Ident(_)) => {
                if matches!(self.peek_n(1), Some(Tokens::Assign)) {
                    let name = match self.advance() {
                        Some(Tokens::Ident(n)) => n,
                        _ => unreachable!(),
                    };
                    self.expect(Tokens::Assign)?;
                    let value = self.parse_expr()?;
                    self.expect(Tokens::Semicolon)?;
                    Ok(Stmt::Assign { name, value })
                } else {
                    let expr = self.parse_expr()?;
                    self.expect(Tokens::Semicolon)?;
                    Ok(Stmt::ExprStmt { expr })
                }
            }
            other => Err(ParseError {
                message: format!("Unexpected token in statement: {:?}", other),
                position: self.pos,
            }),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_term()?;
        while matches!(self.peek(), Some(Tokens::Plus)) {
            self.advance();
            let right = self.parse_term()?;
            left = Expr::Add(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        match self.advance() {
            Some(Tokens::Int(n)) => Ok(Expr::Int(n)),
            Some(Tokens::Str(s)) => Ok(Expr::Str(s)),
            Some(Tokens::Ident(name)) => {
                if matches!(self.peek(), Some(Tokens::ParL)) {
                    self.advance();
                    let mut args = Vec::new();
                    if !matches!(self.peek(), Some(Tokens::ParR)) {
                        loop {
                            args.push(self.parse_expr()?);
                            if matches!(self.peek(), Some(Tokens::Comma)) {
                                self.advance();
                                continue;
                            } else {
                                break;
                            }
                        }
                    }
                    self.expect(Tokens::ParR)?;
                    Ok(Expr::Call { name, args })
                } else {
                    Ok(Expr::Ident(name))
                }
            }
            Some(Tokens::ParL) => {
                let e = self.parse_expr()?;
                self.expect(Tokens::ParR)?;
                Ok(e)
            }
            other => Err(ParseError {
                message: format!("Unexpected token in expression: {:?}", other),
                position: self.pos,
            }),
        }
    }
}

impl Program {
    fn require_main(&self) -> Result<(), String> {
        for f in &self.funcs {
            if f.name == "main" {
                let rt = type_from_str(&f.return_type);
                if rt != Type::None {
                    return Err(format!("Program error: `main` must return `none`, found `{}`", f.return_type));
                }
                return Ok(());
            }
        }
        Err("Program error: no `main` function found".into())
    }

    fn collect_func_sigs(&self) -> Result<HashMap<String, FuncSig>, String> {
        let mut map = HashMap::new();
        for f in &self.funcs {
            if map.contains_key(&f.name) {
                return Err(format!("Duplicate function `{}`", f.name));
            }
            let mut ptys = Vec::new();
            for (_, t) in &f.params {
                let tt = type_from_str(t);
                if tt == Type::Unknown {
                    return Err(format!("Unknown parameter type `{}` in function `{}`", t, f.name));
                }
                ptys.push(tt);
            }
            let ret = type_from_str(&f.return_type);
            if ret == Type::Unknown {
                return Err(format!("Unknown return type `{}` in function `{}`", f.return_type, f.name));
            }
            map.insert(f.name.clone(), FuncSig { params: ptys, ret });
        }
        Ok(map)
    }

    fn infer_expr_type(expr: &Expr, vars: &HashMap<String, Type>, funcs: &HashMap<String, FuncSig>) -> Result<Type, String> {
        match expr {
            Expr::Int(_) => Ok(Type::Int),
            Expr::Str(_) => Ok(Type::Str),
            Expr::Ident(n) => {
                vars.get(n).cloned().ok_or_else(|| format!("Use of undeclared variable `{}`", n))
            }
            Expr::Call { name, args } => {
                let sig = funcs.get(name).ok_or_else(|| format!("Unknown function `{}`", name))?;
                if sig.params.len() != args.len() {
                    return Err(format!("Argument count mismatch calling `{}`: expected {}, got {}", name, sig.params.len(), args.len()));
                }
                for (i, a) in args.iter().enumerate() {
                    let at = Self::infer_expr_type(a, vars, funcs)?;
                    if at != sig.params[i] {
                        return Err(format!("Type mismatch in arg {} calling `{}`: expected {:?}, got {:?}", i + 1, name, sig.params[i], at));
                    }
                }
                Ok(sig.ret.clone())
            }
            Expr::Add(l, r) => {
                let lt = Self::infer_expr_type(l, vars, funcs)?;
                let rt = Self::infer_expr_type(r, vars, funcs)?;
                if lt != Type::Int || rt != Type::Int {
                    return Err("`+` is only defined for int + int".into());
                }
                Ok(Type::Int)
            }
        }
    }

    fn check_types(&self) -> Result<(), String> {
        let funcs = self.collect_func_sigs()?;
        for f in &self.funcs {
            let expected_ret = type_from_str(&f.return_type);
            let mut vars: HashMap<String, Type> = HashMap::new();
            for (pn, pt) in &f.params {
                let t = type_from_str(pt);
                vars.insert(pn.clone(), t);
            }

            let mut has_return = false;

            for stmt in &f.body {
                match stmt {
                    Stmt::VarDecl { name, var_type, value } => {
                        let declared = type_from_str(var_type);
                        let actual = Self::infer_expr_type(value, &vars, &funcs)?;
                        if declared != actual {
                            return Err(format!("Type error: variable `{}` declared as `{:?}` but initialized with `{:?}`", name, declared, actual));
                        }
                        vars.insert(name.clone(), declared);
                    }
                    Stmt::Assign { name, value } => {
                        let vty = vars.get(name).ok_or_else(|| format!("Use of undeclared variable `{}`", name))?.clone();
                        let aty = Self::infer_expr_type(value, &vars, &funcs)?;
                        if vty != aty {
                            return Err(format!("Type error: assign to `{}` of type `{:?}` from `{:?}`", name, vty, aty));
                        }
                    }
                    Stmt::Print { expr } => {
                        let et = Self::infer_expr_type(expr, &vars, &funcs)?;
                        if et != Type::Int && et != Type::Str {
                            return Err(format!("`print` only supports int or str expressions, got `{:?}`", et));
                        }
                    }
                    Stmt::ExprStmt { expr } => {
                        let _ = Self::infer_expr_type(expr, &vars, &funcs)?;
                    }
                    Stmt::Return { expr } => {
                        has_return = true;
                        match expr {
                            None => {
                                if expected_ret != Type::None {
                                    return Err(format!("Type error: function `{}` must return `{:?}` but returns nothing", f.name, expected_ret));
                                }
                            }
                            Some(e) => {
                                let actual = Self::infer_expr_type(e, &vars, &funcs)?;
                                if actual != expected_ret {
                                    return Err(format!("Type error in function `{}`: expected return `{:?}`, got `{:?}`", f.name, expected_ret, actual));
                                }
                            }
                        }
                    }
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
        Stmt::VarDecl { name, var_type, value } => {
            let ty = type_from_str(var_type);
            format!("let mut {}: {} = {};", name, rust_type(&ty), gen_expr(value))
        }
        Stmt::Assign { name, value } => format!("{} = {};", name, gen_expr(value)),
        Stmt::Print { expr } => format!("println!(\"{{}}\", {});", gen_expr(expr)),
        Stmt::Return { expr } => {
            match expr {
                None => "return;".to_string(),
                Some(e) => format!("return {};", gen_expr(e)),
            }
        }
        Stmt::ExprStmt { expr } => format!("{};", gen_expr(expr)),
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

    let prog = match parser.parse_program() {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Parse failed: {} at token {}", e.message, e.position);
            std::process::exit(1);
        }
    };

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
