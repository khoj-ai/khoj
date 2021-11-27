var showConfig = document.getElementById("show-config");

showConfig.addEventListener("click", () => {
    var configForm = document.getElementById("config-form");
    fetch("/config")
        .then(response => response.json())
        .then(data => { 
            configForm.style.display = "block";
            processChildren(configForm, data);
        });
});

function processChildren(element, data) {
    for (let key in data) {
        var child = document.createElement("div");
        child.id = key;
        child.className = "config-element";
        child.appendChild(document.createTextNode(key + ": "));
        if (data[key] === Object(data[key])) {
            console.log(key, data[key]);
            processChildren(child, data[key]);
        } else {
            child.textContent+=data[key];
        }
        element.appendChild(child);
    }
}