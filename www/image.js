function updateBgImage() {
    if ($("li.active")[0].firstElementChild.dataset.value == "Home") {
        document.body.style.backgroundImage = "tatry3.jpg"
    } else {
        document.body.style.background = "transparent"
    }   
}

$("ul.nav").click(updateBgImage)
