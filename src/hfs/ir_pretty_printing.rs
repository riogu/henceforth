use colored::{Colorize, CustomColor};

const KEYWORD_COLOR: CustomColor = CustomColor::new(203, 166, 247); // mauve
const TYPE_COLOR: CustomColor = CustomColor::new(249, 226, 175); // yellow
const PUNCTUATION_COLOR: CustomColor = CustomColor::new(147, 153, 178); // overlay2
const VAR_COLOR: CustomColor = CustomColor::new(243, 139, 168); // red
const OPERATOR_COLOR: CustomColor = CustomColor::new(137, 220, 235); // sky
const BLOCK_COLOR: CustomColor = CustomColor::new(148, 226, 213); // teal
const LITERAL_COLOR: CustomColor = CustomColor::new(250, 179, 135); // peach
const IDENT_COLOR: CustomColor = CustomColor::new(186, 194, 222); // subtext0

fn fix_pre_punct_spacing(line: &str, punct: char) -> String {
    let chars: Vec<char> = line.chars().collect();
    let mut out = String::with_capacity(line.len());

    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];

        if c == punct {
            while out.ends_with(' ') {
                out.pop();
            }
            out.push(c);
            i += 1;
            continue;
        }

        out.push(c);
        i += 1;
    }

    out
}

fn fix_punct_spacing(line: &str, punct: char) -> String {
    let chars: Vec<char> = line.chars().collect();
    let mut out = String::with_capacity(line.len() + 1);

    for i in 0..chars.len() {
        let c = chars[i];
        out.push(c);

        if c == punct {
            let next = chars.get(i + 1);

            if let Some(n) = next {
                if !n.is_whitespace() {
                    out.push(' ');
                }
            }
        }
    }

    out
}

fn colorize(line: String) -> String {
    let mut result = String::new();
    let mut chars = line.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        let remaining = &line[i..];

        // String literals
        if c == '"' {
            let end = remaining[1..].find('"').map(|e| e + 2).unwrap_or(remaining.len());
            let literal = &remaining[..end];
            result.push_str(&literal.custom_color(LITERAL_COLOR).to_string());
            for _ in 1..end {
                chars.next();
            }
            continue;
        }

        // Number literals
        if c.is_ascii_digit() {
            if let Some(prev) = result.chars().last() {
                if prev.is_alphanumeric() || prev == '%' || prev == '_' {
                    result.push(c);
                    continue;
                }
            }
            let len = remaining.chars().take_while(|c| c.is_ascii_digit() || *c == '.').map(|c| c.len_utf8()).sum::<usize>();
            result.push_str(&remaining[..len].custom_color(LITERAL_COLOR).to_string());
            for _ in 1..len {
                chars.next();
            }
            continue;
        }

        // Vars
        if c == '%' {
            let ident_len =
                remaining[1..].chars().take_while(|c| c.is_alphanumeric() || *c == '_').map(|c| c.len_utf8()).sum::<usize>();
            if ident_len > 0 {
                result.push_str(&remaining[..ident_len + 1].custom_color(VAR_COLOR).to_string());
                for _ in 0..ident_len {
                    chars.next();
                }
                continue;
            }
        }

        // Identifiers / keywords / types / blocks
        if c.is_alphabetic() || c == '_' {
            let len = remaining.chars().take_while(|c| c.is_alphanumeric() || *c == '_').map(|c| c.len_utf8()).sum::<usize>();
            let word = &remaining[..len];
            let color = match word {
                "fn" | "phi" | "load" | "store" | "branch" | "alloca" | "retval" | "call" | "load_element" | "return"
                | "jump" => KEYWORD_COLOR,
                "i32" | "f32" | "str" | "bool" => TYPE_COLOR,
                "true" | "false" => LITERAL_COLOR,
                _ => {
                    // block labels: identifier followed by colon
                    if remaining[len..].trim_start().starts_with(':') && !line.trim_start().starts_with("fn") {
                        BLOCK_COLOR
                    } else {
                        IDENT_COLOR
                    }
                },
            };
            result.push_str(&word.custom_color(color).to_string());
            for _ in 1..len {
                chars.next();
            }
            continue;
        }

        // Operators
        let op_match = ["==", "!=", ">=", "<=", "||", "&&", "->", ">", "<", "!", "+", "-", "*", "/", "%"]
            .iter()
            .find(|op| remaining.starts_with(*op));
        if let Some(op) = op_match {
            result.push_str(&op.custom_color(OPERATOR_COLOR).to_string());
            for _ in 1..op.len() {
                chars.next();
            }
            continue;
        }

        // Punctuation
        let punct_match = ["(", ")", "[", ":", "]", "{", "}"].iter().find(|p| remaining.starts_with(*p));
        if let Some(p) = punct_match {
            result.push_str(&p.custom_color(PUNCTUATION_COLOR).to_string());
            continue;
        }

        result.push(c);
    }

    result
}

pub fn prettify_ir(ir: String) -> String {
    ir.lines()
        .map(|line| {
            let mut line = fix_punct_spacing(line, ':');
            line = fix_punct_spacing(&line, ',');
            line = fix_pre_punct_spacing(&line, ':');
            line = fix_pre_punct_spacing(&line, ',');

            if line.trim().starts_with("fn") || line.trim().starts_with("}") {
                colorize(line)
            } else if line.trim().ends_with(":") {
                colorize("  ".to_string() + &line)
            } else {
                colorize(line)
            }
        })
        .collect::<Vec<String>>()
        .join("\n")
}
