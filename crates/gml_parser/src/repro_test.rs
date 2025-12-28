
#[cfg(test)]
mod reproduction_test {
    use super::*;
    use crate::ast::*;

    #[test]
    fn test_parse_struct_simple() {
        let source = "x = {a: 0, b: 0};";
        let parser = Parser::new(source);
        let result = parser.parse();
        assert!(result.is_ok(), "Failed to parse simple struct: {:?}", result.err());
    }

    #[test]
    fn test_parse_anonymous_function_assignment() {
        let source = "self.func = function() { return 0; };";
        let parser = Parser::new(source);
        let result = parser.parse();
        assert!(result.is_ok(), "Failed to parse anonymous function: {:?}", result.err());
    }
    
    #[test]
    fn test_parse_anonymous_function_in_struct() {
         let source = "x = { f: function() { } };";
        let parser = Parser::new(source);
        let result = parser.parse();
        assert!(result.is_ok(), "Failed to parse anonymous function in struct: {:?}", result.err());
    }
}
