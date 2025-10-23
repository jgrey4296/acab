"use strict";

const ToggleAll = () => {
    const event = new MouseEvent("click", {
        view: window,
        bubbles: true,
        cancelable: true,
    });
    const togglerElements = document.querySelectorAll("img.toggler");
    togglerElements.forEach((el) => el.dispatchEvent(event));
};

const ToggleAllInit = () => {
    console.log("Creating expand all event listener");
    const button = document.querySelector("#expandall");
    if (button != null) {
        button.addEventListener("click", (event) => ToggleAll());
    }

};

_ready(ToggleAllInit);
