var showConfig = document.getElementById("show-config");
var rawConfig = {};

var configForm = document.getElementById("config-form");

var emptyValueDefault = "🖊️";

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
            console.log(rawConfig);
            const response = fetch("/config", {
                method: "POST",
                credentials: "same-origin",
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(rawConfig)
            }).then(response => response.json())
            .then((data) => console.log(data));
        });
});

function processChildren(element, data) {
    for (let key in data) {
        var child = document.createElement("div");
        child.id = key;
        child.className = "config-element";
        child.appendChild(document.createTextNode(key + ": "));
        if (data[key] === Object(data[key]) && !Array.isArray(data[key])) {
            child.className+=" config-title";
            processChildren(child, data[key]);
        } else {
            child.appendChild(createValueNode(data, key));
        }
        element.appendChild(child);
    }
}

function makeElementEditable(original, data, key) {
    original.addEventListener("click", () => {
        var inputNewText = document.createElement("input");
        inputNewText.type = "text";
        inputNewText.className = "config-element-edit";
        inputNewText.value = original.textContent;
        fixInputOnFocusOut(inputNewText, data, key);
        original.parentNode.replaceChild(inputNewText, original);
        inputNewText.focus();
    });
}

function createValueNode(data, key) {
    var valueElement = document.createElement("span");
    valueElement.className = "config-element-value";
    valueElement.textContent = !data[key] ? emptyValueDefault : data[key];
    makeElementEditable(valueElement, data, key);
    return valueElement;
}

function fixInputOnFocusOut(original, data, key) {
    original.addEventListener("blur", () => {
        data[key] = (!!data[key] && original.value != emptyValueDefault) ? original.value : "";
        original.parentNode.replaceChild(createValueNode(data, key), original);
    })
}
