import moo from "moo";

export interface Position {
  line: number;
  col: number;
}

export interface Token {
  start: Position;
  end: Position;
  type?: string;
  value: string;
}

export const lexer = moo.compile({
  ws: /[ \t]+?/,
  comment: /--.*?$/,
  multiLineComment: { match: /\{-[^]*?-}/, lineBreaks: true },
  identifier: /[\w][\w\d'\-]*/,
  punctuation: ["(", ")", "->", "::", ":", "=>", "=", "*", "\\"],
  nl: { match: "\n", lineBreaks: true },
});

export function tokenStart(token: moo.Token): Position {
  return {
    line: token.line,
    col: token.col - 1,
  };
}

export function tokenEnd(token: moo.Token): Position {
  return {
    line: token.line,
    col: token.col + token.text.length - 1,
  };
}

export function convertToken(token: moo.Token): Token {
  return {
    type: token.type,
    value: token.value,
    start: tokenStart(token),
    end: tokenEnd(token),
  };
}

export function convertTokenId(data: moo.Token[]): Token {
  return convertToken(data[0]);
}
