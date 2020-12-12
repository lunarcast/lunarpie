import { lexer, convertToken } from "./lexer";
import { readFileSync } from "fs";

const content = readFileSync("examples/test.lpi").toString();

lexer.reset(content);

for (const token of lexer) {
  console.log(convertToken(token));
}
