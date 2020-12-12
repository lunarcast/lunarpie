import { lexer, convertToken, withIndentation } from "./lexer";
import { readFileSync } from "fs";
import { main } from "../output/Main";

const content = readFileSync("examples/test.lpi").toString();

lexer.reset(content);

const tokens = withIndentation();
const lastToken = tokens[tokens.length - 1];

tokens.push({
  end: lastToken.end,
  start: lastToken.start,
  value: "__eof",
  type: "eof",
  indentation: Infinity,
});

main(tokens)();
