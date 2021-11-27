var showConfig = document.getElementById("show-config");
showConfig.addEventListener("click", () => {
    fetch("/config")
        .then(response => response.json())
        .then(data => { 
            for (let key in data) {
                console.log('key: ', key);
                console.log(data[key]);
            }
        });
});