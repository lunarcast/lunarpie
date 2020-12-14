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

const lexer = moo.compile({
  ws: /[ \t]+?/,
  comment: /--.*?$/,
  multiLineComment: { match: /\{-[^]*?-}/, lineBreaks: true },
  keyword: ["assume"],
  identifier: /[\w][\w\d'\-]*/,
  punctuation: ["(", ")", "->", "::", ":", "=>", "=", "*", "\\"],
  nl: { match: "\n", lineBreaks: true },
});

function tokenStart(token: moo.Token): Position {
  return {
    line: token.line,
    column: token.col - 1,
  };
}

function tokenEnd(token: moo.Token): Position {
  return {
    line: token.line,
    column: token.col + token.text.length - 1,
  };
}

function convertToken(token: moo.Token): Token {
  return {
    type: token.type,
    value: token.value,
    start: tokenStart(token),
    end: tokenEnd(token),
  };
}

function withIndentation(): IndentedToken[] {
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

export function lex(text: string) {
  lexer.reset(text);

  const tokens = withIndentation();
  const lastToken = tokens[tokens.length - 1];

  tokens.push({
    end: lastToken.end,
    start: lastToken.start,
    value: "__eof",
    type: "eof",
    indentation: Infinity,
  });

  return tokens;
}
