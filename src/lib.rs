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
    pub fn expect_int(&self) -> Result<(), EGError> {
        if let Value::Int(_) = self {
            Ok(())
        } else {
            Err(EGError::StaticBorrow("value is not an integer"))
        }
    }

    pub fn expect_float(&self) -> Result<(), EGError> {
        if let Value::Float(_) = self {
            Ok(())
        } else {
            Err(EGError::StaticBorrow("value is not a float number"))
        }
    }

    pub fn expect_str(&self) -> Result<(), EGError> {
        if let Value::String(_) = self {
            Ok(())
        } else {
            Err(EGError::StaticBorrow("value is not a string"))
        }
    }

    pub fn expect_list(&self) -> Result<(), EGError> {
        if let Value::List(_) = self {
            Ok(())
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
                            Err(EGError::Owned(format!("{} does not exist", TreeNode::chain_to_string(chain))))
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

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
