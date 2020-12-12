import moo from "moo";

export interface Position {
  line: number;
  column: number;
}

export interface Token {
  start: Position;
  end: Position;
  type?: string;
  value: string;
}

export interface IndentedToken extends Token {
  indentation: number;
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
    column: token.col - 1,
  };
}

export function tokenEnd(token: moo.Token): Position {
  return {
    line: token.line,
    column: token.col + token.text.length - 1,
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

export function withIndentation(): IndentedToken[] {
  const result: IndentedToken[] = [];

  let indentation = 0;
  let startOfLine = true;

  for (const rawToken of lexer) {
    const token = convertToken(rawToken);

    if (token.type === "nl") {
      indentation = 0;
      startOfLine = true;
      continue;
    }

    if (token.type === "ws") {
      if (startOfLine) indentation += token.value.length;
      continue;
    }

    if (token.type === "comment") continue;

    if (token.type === "multiLineComment") {
      startOfLine = false;
      continue;
    }

    result.push({
      ...token,
      indentation,
    });
  }

  return result;
}
