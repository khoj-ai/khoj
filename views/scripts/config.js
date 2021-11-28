var showConfig = document.getElementById("show-config");
var rawConfig = {};

var configForm = document.getElementById("config-form");
fetch("/config")
    .then(response => response.json())
    .then(data => {
        rawConfig = data;
        configForm.style.display = "block";
        processChildren(configForm, data);

        var submitButton = document.createElement("button");
        submitButton.type = "submit";
        submitButton.innerHTML = "update";
        configForm.appendChild(submitButton);

        configForm.addEventListener("submit", (event) => {
            event.preventDefault();
            console.log("submitted!");
            console.log(event);
            console.log(configForm.children);
            console.log(configForm.childNodes);
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
            makeElementEditable(value, data, key);
            child.appendChild(value);
        }
        element.appendChild(child);
        // data[key] = "wassup?";
    }
    console.log(data);
    console.log(rawConfig);
}

function makeElementEditable(original, data, key) {
    original.addEventListener("click", (event) => {
        var inputNewText = document.createElement("input");
        inputNewText.type = "text";
        inputNewText.className = "config-element-edit";
        inputNewText.value = original.textContent;
        fixInputOnFocusOut(inputNewText, data, key);
        original.parentNode.replaceChild(inputNewText, original);
        inputNewText.focus();
    });
}

function fixInputOnFocusOut(original, data, key) {
    original.addEventListener("blur", () => {
        console.log(original);
        var value = document.createElement("span");
        value.id = original.id;
        value.textContent = original.value;
        data[key] = value.textContent;
        console.log(data);
        console.log(rawConfig);
        makeElementEditable(value);
        original.parentNode.replaceChild(value, original);
    })
}

function handleSubmit() {
    submitButton.addEventListener("click", (event) => {
        var submitButton = document.createElement("button");
        submitButton.type = "submit";
    });
    configForm.appendChild(submitButton);
}