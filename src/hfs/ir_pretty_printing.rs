use colored::{Colorize, CustomColor};

// mapped to the theme's actual vim highlight groups, not just picked for contrast
const KEYWORD_COLOR: CustomColor = CustomColor::new(142, 163, 217); // Keyword -> ui_blue_light #8EA3D9
const TYPE_COLOR: CustomColor = CustomColor::new(252, 191, 85); // Type -> yellow #FCBF55
const PUNCTUATION_COLOR: CustomColor = CustomColor::new(151, 167, 210); // Delimiter -> fg_light #97A7D2
const VAR_COLOR: CustomColor = CustomColor::new(205, 212, 232); // fg_dim #CDD4E8
const OPERATOR_COLOR: CustomColor = CustomColor::new(136, 151, 182); // Operator -> ui_grey #8897B6
const BLOCK_COLOR: CustomColor = CustomColor::new(140, 161, 215); // Label -> blue_struct #8CA1D7
const STRING_COLOR: CustomColor = CustomColor::new(88, 154, 143); // String -> green #589A8F
const LITERAL_COLOR: CustomColor = CustomColor::new(224, 180, 187); // Number/Boolean -> pink #E0B4BB
const IDENT_COLOR: CustomColor = CustomColor::new(156, 209, 255); // Function -> blue_light #9CD1FF

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

// shared tokenizer: wrap(text, Some(color)) formats a colored span, wrap(text, None) is used for
// everything else (so an HTML-target wrap can still escape it). ansi/html targets below just plug
// in a different wrap.
fn colorize_with(line: &str, wrap: &dyn Fn(&str, Option<CustomColor>) -> String) -> String {
    let mut result = String::new();
    let mut chars = line.char_indices().peekable();

    while let Some((i, c)) = chars.next() {
        let remaining = &line[i..];

        // String literals
        if c == '"' {
            let end = remaining[1..].find('"').map(|e| e + 2).unwrap_or(remaining.len());
            let literal = &remaining[..end];
            result.push_str(&wrap(literal, Some(STRING_COLOR)));
            for _ in 1..end {
                chars.next();
            }
            continue;
        }

        // Number literals
        if c.is_ascii_digit() {
            if let Some(prev) = result.chars().last() {
                if prev.is_alphanumeric() || prev == '%' || prev == '_' {
                    result.push_str(&wrap(&c.to_string(), None));
                    continue;
                }
            }
            let len = remaining.chars().take_while(|c| c.is_ascii_digit() || *c == '.').map(|c| c.len_utf8()).sum::<usize>();
            result.push_str(&wrap(&remaining[..len], Some(LITERAL_COLOR)));
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
                result.push_str(&wrap(&remaining[..ident_len + 1], Some(VAR_COLOR)));
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
            result.push_str(&wrap(word, Some(color)));
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
            result.push_str(&wrap(op, Some(OPERATOR_COLOR)));
            for _ in 1..op.len() {
                chars.next();
            }
            continue;
        }

        // Punctuation
        let punct_match = ["(", ")", "[", ":", "]", "{", "}"].iter().find(|p| remaining.starts_with(*p));
        if let Some(p) = punct_match {
            result.push_str(&wrap(p, Some(PUNCTUATION_COLOR)));
            continue;
        }

        result.push_str(&wrap(&c.to_string(), None));
    }

    result
}

fn colorize(line: String) -> String {
    colorize_with(&line, &|text, color| match color {
        Some(c) => text.custom_color(c).to_string(),
        None => text.to_string(),
    })
}

fn html_wrap(text: &str, color: Option<CustomColor>) -> String {
    // Graphviz HTML-like labels are strict XML: besides the usual &/</> entities, any raw control
    // byte other than tab/LF/CR (e.g. a literal ESC in an ANSI string literal) is flat-out illegal
    // and fails to parse, so those need a visible, XML-safe stand-in instead.
    let mut escaped = String::with_capacity(text.len());
    for c in text.chars() {
        match c {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '\t' | '\n' | '\r' => escaped.push(c),
            c if (c as u32) < 0x20 => escaped.push_str(&format!("\\x{:02x}", c as u32)),
            c => escaped.push(c),
        }
    }
    match color {
        Some(c) => format!("<font color=\"#{:02x}{:02x}{:02x}\">{escaped}</font>", c.r, c.g, c.b),
        None => escaped,
    }
}

/// Colorizes a block of already-formatted IR lines (e.g. one CFG node's instructions) as a
/// Graphviz HTML-like label body - pass the result as `label=<...>` (angle brackets, not quotes).
pub fn colorize_dot_label(lines: &[String]) -> String {
    lines
        .iter()
        .map(|line| {
            let mut line = fix_punct_spacing(line, ':');
            line = fix_punct_spacing(&line, ',');
            line = fix_pre_punct_spacing(&line, ':');
            line = fix_pre_punct_spacing(&line, ',');
            // every line needs its own trailing ALIGN="LEFT", including the last one - an HTML-like
            // label's final (unterminated) line otherwise falls back to Graphviz's default center
            // alignment, which is why terminators (return/jump/branch) looked indented relative to
            // everything above them.
            format!("{}<BR ALIGN=\"LEFT\"/>", colorize_with(&line, &html_wrap))
        })
        .collect::<String>()
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
