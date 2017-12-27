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
let LineBreaker = require('linebreak');

const aslines = sentence => {
  let breaker = new LineBreaker(sentence),
    ret = [],
    line = '',
    last = 0,
    bk;
  while (bk = breaker.nextBreak()) {
    let token = sentence.slice(last, bk.position);
    if (80 < (line + token).length || bk.required) {
      ret.push(line.replace(/\s*$/,''));
      line = '';
    }
    line += token;
    last = bk.position;
  }
  if (line.length) ret.push(line);
  return ret.join("\n") + "\n";
};

{
  let fh = process.stdin,
    line = '';
  fh.setEncoding('utf8');
  fh.on('readable', () => {
    let chunk = '';
    while (chunk = fh.read()) line += chunk;
  });
  fh.on('end', () => {
    let start = line.indexOf(`"`),
      end = line.length - line.split("").reverse().join("").indexOf(`"`),
      key = line.substr(0,start),
      quoted = line.substr(start,end-start),
      //_ = console.log({start,end,key,quoted,line}),
      trail = line.substr(end),
      raw = eval(quoted),
      lines = raw.split(/\.  (?=[A-Z])/).map(l=>l.replace(/\.*$/,'.')).map(aslines).join(""),
      subbed = "\n" + lines.replace(/`/g, "\\`"),
      out = [key,subbed,trail].join("`");
    process.stdout.write(out);
  });
}
