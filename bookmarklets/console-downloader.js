// from: http://bgrins.github.io/devtools-snippets/#console-save
// via: https://stackoverflow.com/a/19818659/82723
{
  let saveimg = (url, name = null) => {
    let href = new URL(url),
      download = name === null ? href.pathname.split("/").pop() : name,
      e = new MouseEvent("click", { bubbles: true, view: window }),
      props = { href, download };
    Object.assign(document.createElement("a"), props).dispatchEvent(e);
  };
  let save = (data, filename = "console.json") => {
    let out = JSON.stringify(data, null, 1),
      type = "application/json",
      blob = new Blob([out], { type }),
      e = new MouseEvent("click", { bubbles: true, view: window }),
      a = document.createElement("a");
    a.download = filename;
    a.href = window.URL.createObjectURL(blob);
    a.dataset.downloadurl = [type, a.download, a.href].join(":");
    a.dispatchEvent(e);
  };
  Object.assign(console, { save, saveimg });
}
