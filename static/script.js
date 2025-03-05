function toggleTOC() {
  let toc = document.getElementById("table-of-contents");
  if (!toc) return;
  console.log(toc.style.display)
  if (toc.style.display === "none" || toc.style.display === "") {
    toc.style.display = "block";
  } else {
    toc.style.display = "none";
  }
}
