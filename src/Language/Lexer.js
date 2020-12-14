const lexer = require("../../src/lexer");

exports.lexImpl = ({ left, right }) => (text) => {
  try {
    return right(lexer.lex(text));
  } catch (err) {
    return left(err);
  }
};
