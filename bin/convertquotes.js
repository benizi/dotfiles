// Dumb utility for wrapping lines in a JavaScript file.
//
// Turns this:
//
//         html: "some very long text",
//
// into:
//
//         html: `
//     some very
//     long text
//     `,
//
let LineBreaker = require("linebreak");

const aslines = sentence => {
  let breaker = new LineBreaker(sentence),
    ret = [],
    line = "",
    last = 0,
    bk;
  while ((bk = breaker.nextBreak())) {
    let token = sentence.slice(last, bk.position);
    // Don't want to wrap already-hyphenated words:
    if (token.match(/[-\/]$/)) {
      continue;
    }
    // Don't want to wrap incomplete anchor tags:
    if (token.match(/<a /) && !token.match(/">/)) {
      continue;
    }
    if (80 < (line + token).length || bk.required) {
      ret.push(line.replace(/\s*$/, ""));
      line = "";
    }
    line += token;
    last = bk.position;
  }
  if (line.length) ret.push(line);
  return ret.join("\n") + "\n";
};

{
  let wrapLine = line => {
      let start = line.indexOf(`"`),
        end = line.length - line.split("").reverse().join("").indexOf(`"`),
        key = line.substr(0, start),
        quoted = line.substr(start, end - start),
        trail = line.substr(end),
        raw = eval(quoted),
        lines = raw
          .split(/\.  (?=[A-Z])/)
          .map(l => l.replace(/\.*$/, "."))
          .map(aslines)
          .join(""),
        subbed = "\n" + lines.replace(/`/g, "\\`"),
        out = [key, subbed, trail].join("`");
      return out;
    },
    fh = process.stdin,
    file = "",
    wrapFile = () => {
      let lines = file.split(/\n/),
        isAttr = str => str.match(/^ *\w+: "(?:[^\\"]|\\")*",$/),
        isLong = str => str.length > 78 && isAttr(str),
        replaced = lines.map(l => (isLong(l) ? wrapLine(l) : l)),
        out = replaced.join("\n");
      process.stdout.write(out);
    };
  fh.setEncoding("utf8");
  fh.on("readable", () => {
    let chunk = "";
    while ((chunk = fh.read())) file += chunk;
  });
  fh.on("end", () => wrapFile(file));
}
