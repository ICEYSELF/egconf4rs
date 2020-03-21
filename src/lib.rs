use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub enum EGError {
    Owned(String),
    StaticBorrow(&'static str)
}

impl Display for EGError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            EGError::Owned(s) => write!(f, "{}", s),
            &EGError::StaticBorrow(s) => write!(f, "{}", s)
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(std::string::String),
    List(Vec<Value>)
}

impl Value {
    pub fn expect_int(&self) -> Result<i64, EGError> {
        if let Value::Int(i) = self {
            Ok(*i)
        } else {
            Err(EGError::StaticBorrow("value is not an integer"))
        }
    }

    pub fn expect_float(&self) -> Result<f64, EGError> {
        if let Value::Float(f) = self {
            Ok(*f)
        } else {
            Err(EGError::StaticBorrow("value is not a float number"))
        }
    }

    pub fn expect_str(&self) -> Result<&String, EGError> {
        if let Value::String(s) = self {
            Ok(s)
        } else {
            Err(EGError::StaticBorrow("value is not a string"))
        }
    }

    pub fn expect_list(&self) -> Result<&Vec<Value>, EGError> {
        if let Value::List(ls) = self {
            Ok(ls)
        } else {
            Err(EGError::StaticBorrow("value is not a list"))
        }
    }

    pub fn maybe_as_bool(&self) -> Option<bool> {
        match self {
            Value::Int(i) => Some(*i != 0),
            Value::String(s) => Some(s == "true" || s == "True" || s == "1"),
            _ => None
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(lf) => write!(f, "{}", lf),
            Value::String(s) => write!(f, "{}", s),
            Value::List(ls) => {
                write!(f, "[")?;
                for i in 0..ls.len() {
                    write!(f, "{}", ls[i])?;
                }
                if let Some(last) = ls.last() {
                    write!(f, "{}", last)?;
                }
                write!(f, "]")
            }
        }
    }
}

enum TreeNode {
    Leaf { key: String, value: Value },
    Branch { key: Option<String>, children: Vec<TreeNode> },
}

impl TreeNode {
    fn chain_to_string(chain: &[String]) -> String {
        debug_assert!(chain.len() != 0);
        let mut ret = String::new();
        for s in &chain[0..chain.len() - 1] {
            ret.push_str(s);
            ret.push('.');
        }
        ret.push_str(&chain[chain.len() - 1]);
        ret
    }

    fn find(&self, chain: &[String]) -> Result<&Value, EGError> {
        if chain.len() == 0 {
            Err(EGError::StaticBorrow("empty input chain"))
        } else {
            self.find_impl(chain, chain)
        }
    }

    fn insert(&mut self, chain: &[String], value: Value) -> Result<(), EGError> {
        if chain.len() == 0 {
            Err(EGError::StaticBorrow("empty input chain"))
        } else {
            self.insert_impl(chain, chain, value)
        }
    }

    fn find_impl(&self, chain: &[String], orig_chain: &[String]) -> Result<&Value, EGError> {
        match self {
            TreeNode::Leaf{ key, value } => {
                if chain.len() == 0 {
                    Ok(value)
                } else {
                    Err(EGError::Owned(format!("{} is a leaf node and has no children",
                                               TreeNode::chain_to_string(orig_chain))))
                }
            },
            TreeNode::Branch{ key, children} => {
                if chain.len() != 0 {
                    if let Some(child) =
                            children.iter().find(|&node| *node.key() == chain[0]) {
                        child.find_impl(&chain[1..], orig_chain)
                    } else {
                        Err(EGError::Owned(format!("{} does not exist", TreeNode::chain_to_string(chain))))
                    }
                } else {
                    Err(EGError::Owned(format!("{} is a branch node and has no value",
                                               TreeNode::chain_to_string(orig_chain))))
                }
            }
        }
    }

    fn insert_impl(&mut self, chain: &[String], orig_chain: &[String], value: Value) -> Result<(), EGError> {
        match self {
            TreeNode::Leaf { key, value } => {
                let _ = key;
                let _ = value;
                Err(EGError::Owned(format!("{} is already a leaf node",
                                   TreeNode::chain_to_string(orig_chain))))
            },
            TreeNode::Branch { key, children } => {
                match chain.len() {
                    0 => Err(EGError::Owned(format!("{} is already a branch node",
                                                    TreeNode::chain_to_string(orig_chain)))),
                    1 => {
                        if let Some(child) =
                                children.iter().find(|&child| *child.key() == chain[0]) {
                            Err(EGError::Owned(format!("{} already exists", TreeNode::chain_to_string(orig_chain))))
                        } else {
                            children.push(TreeNode::Leaf { key: chain[0].to_string(), value });
                            Ok(())
                        }
                    },
                    _ => {
                        if let Some(child) =
                                children.iter_mut().find(|child| *child.key() == chain[0]) {
                            child.insert_impl(&chain[1..], orig_chain, value)
                        } else {
                            let mut new_child =
                                TreeNode::Branch { key: Some(chain[0].to_string()), children: Vec::new() };
                            assert!(new_child.insert_impl(&chain[1..], orig_chain, value).is_ok());
                            children.push(new_child);
                            Ok(())
                        }
                    }
                }
            }
        }
    }

    fn key(&self) -> &String {
        match self {
            TreeNode::Leaf { key, value } => key,
            TreeNode::Branch { key, children } =>
                key.as_ref().expect("should not fetch the key of root node")
        }
    }
}

#[derive(Debug)]
struct ParseResult(Vec<String>, Value);

struct LineParser<'a> {
    line: &'a [u8],
    cur: usize,
    chain: Vec<String>
}

impl<'a> LineParser<'a> {
    fn new(line: &'a [u8]) -> Self {
        LineParser { line, cur: 0, chain: Vec::new(), }
    }

    fn skip_whitespace(&mut self) {
        while self.cur < self.line.len()
              && self.line[self.cur].is_ascii_whitespace() {
            self.cur += 1;
        }
    }

    fn parse_id_part(&mut self) -> Result<String, EGError> {
        let mut buffer = Vec::new();
        while self.cur < self.line.len()
              && (self.line[self.cur].is_ascii_alphanumeric()
                  || [b'-', b'_', b'?', b'!', b'$', b'@']
                     .iter()
                     .find(|&&ch| ch == self.line[self.cur])
                     .is_some()) {
            buffer.push(self.line[self.cur]);
            self.cur += 1;
        }
        if buffer.len() == 0 {
            return Err(EGError::StaticBorrow("empty identifier"));
        }
        Ok(String::from_utf8_lossy(&buffer[..]).to_string())
    }

    fn parse_number(&mut self) -> Result<Value, EGError> {
        debug_assert!(self.line[self.cur].is_ascii_digit());

        let mut buffer = Vec::new();
        while self.cur < self.line.len() && self.line[self.cur].is_ascii_digit() {
            buffer.push(self.line[self.cur]);
            self.cur += 1;
        }

        if self.cur < self.line.len() && self.line[self.cur] == b'.' {
            self.cur += 1;
            let mut buffer2 = Vec::new();
            while self.cur < self.line.len() && self.line[self.cur].is_ascii_digit() {
                buffer2.push(self.line[self.cur]);
                self.cur += 1;
            }
            if buffer2.len() == 0 {
                return Err(EGError::StaticBorrow("no digit after floating point"));
            }
            buffer.push(b'.');
            buffer.append(&mut buffer2);
            Ok(Value::Float(String::from_utf8_lossy(&buffer).parse().unwrap()))
        } else {
            Ok(Value::Int(String::from_utf8_lossy(&buffer).parse().unwrap()))
        }
    }

    fn parse_string(&mut self) -> Result<Value, EGError> {
        debug_assert!(self.line[self.cur] == b'"');
        self.cur += 1;
        let mut buffer = Vec::new();
        while self.cur < self.line.len() && self.line[self.cur] != b'"' {
            if self.line[self.cur] == b'\\' {
                let peek = self.cur + 1;
                if peek >= self.line.len() {
                    return Err(EGError::StaticBorrow("unexpected end of file"))
                }
                match self.line[peek] {
                    b'\\' => buffer.push(b'\\'),
                    b'"' => buffer.push(b'"'),
                    b'n' => buffer.push(b'\n'),
                    _ => return Err(EGError::Owned(format!("incorrect conversion sequence \\{}", self.line[peek])))
                }
                self.cur += 2;
            } else {
                buffer.push(self.line[self.cur]);
                self.cur += 1;
            }
        }

        if self.cur >= self.line.len() || self.line[self.cur] != b'"' {
            return Err(EGError::StaticBorrow("unclosed string"))
        }
        self.cur += 1;

        Ok(Value::String(String::from_utf8_lossy(&buffer).to_string()))
    }

    fn parse_list(&mut self) -> Result<Value, EGError> {
        debug_assert!(self.line[self.cur] == b'[');
        self.cur += 1;
        self.skip_whitespace();

        let mut values = Vec::new();

        while self.cur < self.line.len() && self.line[self.cur] != b']' {
            let value = self.parse_value()?;
            values.push(value);
            self.skip_whitespace();
            if self.cur < self.line.len()
               && self.line[self.cur] != b','
               && self.line[self.cur] != b']' {
                return Err(EGError::StaticBorrow("missing ',' as value separator"))
            }
            if self.line[self.cur] == b',' {
                self.cur += 1;
            }
            self.skip_whitespace();
        }

        if self.cur >= self.line.len() || self.line[self.cur] != b']' {
            return Err(EGError::StaticBorrow("unclosed list"))
        }
        self.cur += 1;

        Ok(Value::List(values))
    }

    fn parse_value(&mut self) -> Result<Value, EGError> {
        if self.cur >= self.line.len() {
            return Err(EGError::StaticBorrow("unexpected end of file"))
        }

        if self.line[self.cur].is_ascii_digit() {
            self.parse_number()
        } else if self.line[self.cur] == b'"' {
            self.parse_string()
        } else if self.line[self.cur] == b'[' {
            self.parse_list()
        } else {
            Err(EGError::StaticBorrow("wrong value type"))
        }
    }

    fn parse(mut self) -> Result<ParseResult, EGError> {
        self.skip_whitespace();
        let first_part = self.parse_id_part()?;
        self.chain.push(first_part);
        while self.cur < self.line.len() && self.line[self.cur] == b'.'{
            self.cur += 1;
            let part = self.parse_id_part()?;
            self.chain.push(part)
        }

        self.skip_whitespace();
        if self.cur >= self.line.len() || self.line[self.cur] != b'=' {
            return Err(EGError::StaticBorrow("expected '='"))
        }
        self.cur += 1;

        self.skip_whitespace();
        let value = self.parse_value()?;

        Ok(ParseResult(self.chain, value))
    }
}

#[cfg(test)]
mod tests {
    use crate::{TreeNode, Value, LineParser, ParseResult};

    #[test]
    fn test_insert_and_find() {
        let mut root_node = TreeNode::Branch{ key: None, children: Vec::new() };

        let chain_a = ["a".to_string()];
        let chain_b_c = ["b".to_string(), "c".to_string()];
        let chain_b_d_e = ["b".to_string(), "d".to_string(), "e".to_string()];
        let chain_b_d_f = ["b".to_string(), "d".to_string(), "f".to_string()];

        assert!(root_node.insert(&chain_a, Value::Int(42)).is_ok());
        assert!(root_node.insert(&chain_b_c, Value::Float(3.14)).is_ok());
        assert!(root_node.insert(&chain_b_d_e,
                                 Value::String("x".to_string())).is_ok());
        assert!(root_node.insert(&chain_b_d_f,
                                 Value::String("testing".to_string())).is_ok());

        assert_eq!(root_node.find(&chain_a).unwrap().expect_int().unwrap(), 42);
        assert_eq!(root_node.find(&chain_b_c).unwrap().expect_float().unwrap(), 3.14);
        assert_eq!(root_node.find(&chain_b_d_e).unwrap().expect_str().unwrap(), "x");
        assert_eq!(root_node.find(&chain_b_d_f).unwrap().expect_str().unwrap(), "testing");
    }

    #[test]
    fn test_insert_conflict() {
        let mut root_node = TreeNode::Branch { key: None, children: Vec::new() };

        let ok_chains = [
            vec!["a".to_string()],
            vec!["b".to_string(), "c".to_string()],
            vec!["b".to_string(), "d".to_string(), "e".to_string()],
            vec!["b".to_string(), "d".to_string(), "f".to_string()]
        ];

        let err_chains = [
            vec!["a".to_string()],
            vec!["a".to_string(), "b".to_string()],
            vec!["b".to_string(), "d".to_string()],
            vec!["b".to_string(), "d".to_string(), "f".to_string(), "nmsl".to_string()]
        ];

        for chain in ok_chains.iter() {
            root_node.insert(&chain[..], Value::Int(42)).unwrap();
        }

        for chain in err_chains.iter() {
            assert!(root_node.insert(&chain[..], Value::Int(42)).is_err());
        }
    }

    #[test]
    fn test_parse_line() {
        let result =
            LineParser::new(b"a.b.c.d = [  101 , \"string\", [\"91\\n\", 91], 324.5]  ").parse();
        let ParseResult(chain, value) = result.unwrap();
        assert_eq!(chain.len(), 4);
        assert_eq!(chain[0], "a");
        assert_eq!(chain[1], "b");
        assert_eq!(chain[2], "c");
        assert_eq!(chain[3], "d");

        if let Value::List(ls) = &value {
            if let Value::Int(101) = ls[0] {} else { panic!() }
            if let Value::String(s) = &ls[1] {
                if s != "string" { panic!() }
            } else {
                panic!()
            }
            if let Value::List(_) = ls[2] {} else { panic!() }
            if let Value::Float(324.5) = ls[3] {} else { panic!() }
        } else {
            panic!("should be a list!")
        }
    }
}
