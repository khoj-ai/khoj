var showConfig = document.getElementById("show-config");
var configForm = document.getElementById("config-form");
showConfig.addEventListener("click", () => {
    fetch("/config")
        .then(response => response.json())
        .then(data => { 
            configForm.style.display = "block";
            for (let key in data) {
                console.log('key: ', key);
                console.log(data[key]);
            }
        });
});