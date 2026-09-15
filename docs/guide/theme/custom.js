document.addEventListener("DOMContentLoaded", () => {
  const btn = document.createElement("a");
  btn.href = "https://riogu.github.io/henceforth";
  btn.className = "hover-button";
  btn.textContent = "Back to Main Page ↗";
  btn.target = "_blank";
  document.body.appendChild(btn);
});
