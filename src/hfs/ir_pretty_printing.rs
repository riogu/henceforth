use aho_corasick::AhoCorasick;
use colored::{Colorize, CustomColor};

const KEYWORD_COLOR: CustomColor = CustomColor::new(150, 200, 150);

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

fn color_keyword(line: String) -> String {
    let keywords = vec!["fn".to_string(), "phi".to_string(), "load".to_string(), "store".to_string(), "branch".to_string()];
    let colored_keywords: Vec<String> = keywords.iter().map(|kw| kw.custom_color(KEYWORD_COLOR).to_string()).collect();
    let ac = AhoCorasick::new(&keywords).unwrap();

    let mut wtr = vec![];
    ac.try_stream_replace_all(line.as_bytes(), &mut wtr, &colored_keywords).unwrap();
    String::from_utf8(wtr).unwrap()
}

fn colorize(mut line: String) -> String {
    line = color_keyword(line);

    line
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
                colorize("    ".to_string() + &line)
            }
        })
        .collect::<Vec<String>>()
        .join("\n")
}
