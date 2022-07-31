// Retrieve elements from the DOM.
var showConfig = document.getElementById("show-config");
var configForm = document.getElementById("config-form");
var regenerateButton = document.getElementById("config-regenerate");

// Global variables.
var rawConfig = {};
var emptyValueDefault = "🖊️";

/**
 * Fetch the existing config file.
 */
fetch("/config/data")
    .then(response => response.json())
    .then(data => {
        rawConfig = data;
        configForm.style.display = "block";
        processChildren(configForm, data);

        var submitButton = document.createElement("button");
        submitButton.type = "submit";
        submitButton.innerHTML = "update";
        configForm.appendChild(submitButton);

        // The config form's submit handler.
        configForm.addEventListener("submit", (event) => {
            event.preventDefault();
            console.log(rawConfig);
            fetch("/config/data", {
                method: "POST",
                credentials: "same-origin",
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify(rawConfig)
            })
            .then(response => response.json())
            .then(data => console.log(data));
        });
});

/**
 * The click handler for the Regenerate button.
 */
regenerateButton.addEventListener("click", (event) => {
    event.preventDefault();
    regenerateButton.style.cursor = "progress";
    regenerateButton.disabled = true;
    fetch("/regenerate")
        .then(response => response.json())
        .then(data => {
            regenerateButton.style.cursor = "pointer";
            regenerateButton.disabled = false;
            console.log(data);
        });
})

/**
 * Adds config elements to the DOM representing the sub-components 
 * of one of the fields in the raw config file.
 * @param {the parent element} element 
 * @param {the data to be rendered for this element and its children} data 
 */
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

/**
 * Takes an element, and replaces it with an editable 
 * element with the same data in place.
 * @param {the original element to be replaced} original 
 * @param {the source data to be rendered for the new element} data 
 * @param {the key for this input in the source data} key 
 */
function makeElementEditable(original, data, key) {
    original.addEventListener("click", () => {
        var inputNewText = document.createElement("input");
        inputNewText.type = "text";
        inputNewText.className = "config-element-edit";
        inputNewText.value = (original.textContent == emptyValueDefault) ? "" : original.textContent;
        fixInputOnFocusOut(inputNewText, data, key);
        original.parentNode.replaceChild(inputNewText, original);
        inputNewText.focus();
    });
}

/**
 * Creates a node corresponding to the value of a config element.
 * @param {the source data} data 
 * @param {the key corresponding to this node's data} key 
 * @returns A new element which corresponds to the value in some field.
 */
function createValueNode(data, key) {
    var valueElement = document.createElement("span");
    valueElement.className = "config-element-value";
    valueElement.textContent = !data[key] ? emptyValueDefault : data[key];
    makeElementEditable(valueElement, data, key);
    return valueElement;
}

/**
 * Replaces an existing input element with an element with the same data, which is not an input. 
 * If the input data for this element was changed, update the corresponding data in the raw config.
 * @param {the original element to be replaced} original 
 * @param {the source data} data 
 * @param {the key corresponding to this node's data} key 
 */
function fixInputOnFocusOut(original, data, key) {
    original.addEventListener("blur", () => {
        data[key] = (original.value != emptyValueDefault) ? original.value : "";
        original.parentNode.replaceChild(createValueNode(data, key), original);
    })
}
