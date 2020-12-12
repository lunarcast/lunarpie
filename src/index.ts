import { lexer, convertToken } from "./lexer";
import { readFileSync } from "fs";
import { main } from "../output/Main";

const content = readFileSync("examples/test.lpi").toString();

lexer.reset(content);

const skipped = new Set(["comment", "multiLineComment", "nl", "ws"]);
const tokens = [...lexer].map(convertToken).filter((t) => !skipped.has(t.type));
const lastToken = tokens[tokens.length - 1];

tokens.push({
  end: lastToken.end,
  start: lastToken.start,
  value: "",
  type: "eof",
});

main(tokens)();
