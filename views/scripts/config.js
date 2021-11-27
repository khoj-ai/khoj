var showConfig = document.getElementById("show-config");
var rawConfig = {};

showConfig.addEventListener("click", () => {
    var configForm = document.getElementById("config-form");
    fetch("/config")
        .then(response => response.json())
        .then(data => {
            rawConfig = data;
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
            processChildren(child, data[key]);
        } else {
            var value = document.createElement("span");
            value.id = key+"-value";
            value.textContent = data[key];
            createEditButton(value);
            value.addEventListener("click", (event) => {
                var inputNewText = document.createElement("input");
                inputNewText.type = "text";
                inputNewText.class = "config-element-edit";
                inputNewText.id = key+"-value";
                console.log(value.parentNode);
                console.log(value);
                child.replaceChild(inputNewText, value);
                console.log(event);
            });
            child.appendChild(value);
        }
        element.appendChild(child);
    }
}

function createEditButton(parent) {
    var editButton = document.createElement("button");
    editButton.type = "button";
    editButton.className = "config-edit-button";
    editButton.textContent = "🖊️";
    editButton.id = "parentId-" + parent.id;
    // console.log(parent);
    editButton.addEventListener("click", (event) => {
        var inputNewText = document.createElement("input");
        inputNewText.type = "text";
        inputNewText.class = "config-element-edit";
        parent.parentNode.replaceChild(inputNewText, parent);
        // console.log(event);
    })
    // console.log("edit button", editButton);
    parent.appendChild(editButton);
}