{
  let loadLib = new Promise(resolve => {
    if (typeof tldjs !== "undefined") {
      resolve(tldjs);
      return;
    }
    let xhr = new XMLHttpRequest();
    xhr.open("GET", "https://wzrd.in/standalone/tldjs");
    xhr.responseType = "text";
    xhr.onload = () => {
      if (xhr.readyState != xhr.DONE) return;
      if (xhr.status != 200) return;
      let b = new Blob([xhr.responseText], { type: "application/javascript" });
      let s = document.createElement("script");
      s.setAttribute("src", URL.createObjectURL(b));
      s.onload = () => resolve(tldjs);
      document.head.appendChild(s);
    };
    xhr.send();
  });
  loadLib.then(tldjs => prompt("TLD:", tldjs.getDomain(window.location.href)));
}
