// from: http://bgrins.github.io/devtools-snippets/#console-save
// via: https://stackoverflow.com/a/19818659/82723
{
  let saver = (data, filename = "console.json") => {
    let out = JSON.stringify(data, null, 1),
      type = "application/json",
      blob = new Blob([out], { type }),
      e = document.createEvent("MouseEvents"),
      a = document.createElement("a");
    a.download = filename;
    a.href = window.URL.createObjectURL(blob);
    a.dataset.downloadurl = [type, a.download, a.href].join(":");
    e.initMouseEvent(
      "click",
      true,
      false,
      window,
      0,
      0,
      0,
      0,
      0,
      false,
      false,
      false,
      false,
      0,
      null
    );
    a.dispatchEvent(e);
  };
  saver({ a: 1, b: 2 });
}
