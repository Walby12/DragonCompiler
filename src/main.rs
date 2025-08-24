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

fn main() {
    let c = parse(String::from("func hi: int () { return 12; }"));
    println!("{:?}", c);
}
