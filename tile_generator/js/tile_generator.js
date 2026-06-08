(function() {
const pixelMapElem = document.getElementById('pixel_map');
const paletteElem = document.getElementById('palette');
const previewElem = document.getElementById('preview');
const memoryContentElem = document.getElementById('memory_content');
const downloadButtonElem = document.getElementById('download_button');
const uploadButtonElem = document.getElementById('upload_button');
const clearButtonElem = document.getElementById('clear_button');
const fileNameElem = document.getElementById('file_name');
const copyElem = document.getElementById('copy');
const toastBody = document.getElementById('toast_body');

//var pixelSelected = 0;
var pixelElem = new Array();

const mapWidth = 32;
const mapHeight = 32;
//colourNumber map: 0 to 63 are colours, -1 is transparent, -2 is undefined color
const paletteWidth = 4;
const colourCount = 64; //Excluding transparent

const colours = [];
const selected = [];

for (let i = 0; i < mapWidth * mapHeight; i++) {
    colours.push(-1);
    selected.push(i == 0);
}
var lastPixelSelected = 0;

function colourToBinary(colourNumber){
    if (colourNumber >= 0 && colourNumber <= 63) {
        var bin = colourNumber.toString(2);
        bin = "0000000".substr(bin.length) + bin;
        return bin;
    } else if (colourNumber == -1) {
        return "1000000";
    } else{
        return "1111111";
    }
}

function binaryToColour(binayColour){
    if (binayColour.length != 7){
        //console.log("F1 " + binayColour.length + " <" + binayColour + ">");
        return parseInt("1111111", 2);
    }
    bits = binayColour.split('');
    for (let i = 0; i < bits.length; i++){
        if (bits[i] != '0' && bits[i] != '1'){
            //console.log("F2");
            return parseInt("1111111", 2);
        }
    }
    //console.log(binayColour + " " + parseInt(binayColour, 2))
    let result = parseInt(binayColour, 2);
    if (result == 64) {
        return -1;
    } else {
        return result;
    }
}

function colourToHex(colourNumber){
    if (colourNumber >= 0 && colourNumber <= 63) {
        binary = colourToBinary(colourNumber);
        red_2bin = binary.slice(1,3);
        green_2bin = binary.slice(3,5);
        blue_2bin = binary.slice(5,7);

        red_4bin = red_2bin + red_2bin;
        green_4bin = green_2bin + green_2bin;
        blue_4bin = blue_2bin + blue_2bin;

        red_hex = parseInt(red_4bin , 2).toString(16).toUpperCase();
        green_hex = parseInt(green_4bin , 2).toString(16).toUpperCase();
        blue_hex = parseInt(blue_4bin , 2).toString(16).toUpperCase();

        return (red_hex + red_hex + green_hex + green_hex + blue_hex + blue_hex);
    } else{
        return "FFFFFF"; //Return white if transparent or undefined
    }
}

function generateIDPixelElement(idNumber) {
    const id = document.createElementNS('http://www.w3.org/2000/svg','svg');
    id.classList.add("m-0");
    id.classList.add("p-0");
    id.setAttribute("width", "22");
    id.setAttribute("height", "22");
    //id.setAttribute("style", "cursor: none;");
    let htmlToInsert =
    `
        <rect x="0" y="0" width="22" height="22" stroke="red" stroke-width="0px" fill="white"/>
        <text x="50%" y="50%" dominant-baseline="middle" text-anchor="middle" font-size="0.75em"> ${idNumber} </text>
    `
    //pixel.classList.add('pixel');
    //pixel.addEventListener('click', () => togglePixelColor(pixel));
    id.insertAdjacentHTML('beforeend', htmlToInsert);
    return id;
}

function pixelOnClick(id){
    //var cmd_held = window.event.metaKey;
    //console.log(cmd_held);
    //if (window.event.ctrlKey || cmd_held) {
    if (window.event.ctrlKey) {
        //console.log("Ctrl + Click pixel: " + id);
        if(selected[id]){
            selected[id] = false;
        } else {
            selected[id] = true;
        }
        lastPixelSelected = id;
        let pixel = document.getElementById("pixel-" + String(id));
        pixel.innerHTML = generatePixelSvgContent(id);
    } else if (window.event.shiftKey) {
        //console.log("Shift + Click pixel: " + id);
        //Deselect previously selected
        for (let i = 0; i < selected.length; i++){
           if (selected[i]){
               //console.log("Deselect pixel: " + i);
               selected[i] = false;
               let pixel = document.getElementById("pixel-" + String(i));
               pixel.innerHTML = generatePixelSvgContent(i);
           }
        }
        lastPixelSelectedRow = Math.floor(lastPixelSelected/mapWidth);
        lastPixelSelectedCol = lastPixelSelected % mapWidth;
        idRow = Math.floor(id/mapWidth);
        idCol = id % mapWidth;
        minCol = Math.min(lastPixelSelectedCol, idCol);
        maxCol = Math.max(lastPixelSelectedCol, idCol);
        minRow = Math.min(lastPixelSelectedRow, idRow);
        maxRow = Math.max(lastPixelSelectedRow, idRow);
        for (let col = minCol; col <= maxCol; col++){
            for (let row = minRow; row <= maxRow; row++){
                targetId = col + mapWidth * row;
                selected[targetId] = true;
                let pixel = document.getElementById("pixel-" + String(targetId));
                pixel.innerHTML = generatePixelSvgContent(targetId);
            }
        }
   } else {
        //console.log("Click pixel: " + id);
        //Deselect previously selected
        for (let i = 0; i < selected.length; i++){
            if (selected[i]){
                //console.log("Deselect pixel: " + i);
                selected[i] = false;
                let pixel = document.getElementById("pixel-" + String(i));
                pixel.innerHTML = generatePixelSvgContent(i);
            }
        }
        //Select new one
		selected[id] = true;
		lastPixelSelected = id;
		let pixel = document.getElementById("pixel-" + String(id));
		pixel.innerHTML = generatePixelSvgContent(id);
    }
}

function palColourOnClick(colour){
    //console.log("Click palColour: " + colour);
    let changed = false;
    for (let i = 0; i < selected.length; i++){
        if (selected[i]){
            colours[i] = colour;
            let pixel = document.getElementById("pixel-" + String(i));
            pixel.innerHTML = generatePixelSvgContent(i);
            changed = true;
        }
    }
    if(changed){
        updateMemoryContent();
        updatePreview();
    }
}

function generatePixelSvgContent(pixelID){
    let colour = colours[pixelID];
    let htmlToInsert = "";
    //htmlToInsert += `<rect x="0" y="0" rx="0" ry="0" width="22" height="22" stroke="black" stroke-width="0px" fill="#F0F0F0" opacity="1"/>`
    if (selected[pixelID]){
        htmlToInsert += `<rect x="1.5" y="1.5" rx="1" ry="1" width="18.5" height="18.5" stroke="black" stroke-width="3px" fill="#` + colourToHex(colour) + `" opacity="1"/>`
    } else {
        if (colour < 0 || colour > 63){
            htmlToInsert += `<rect x="1.5" y="1.5" rx="2" ry="2" width="19" height="19" stroke="black" stroke-width="1px" fill="#` + colourToHex(colour) + `" opacity="1"/>`
        } else if (colour == 63){
            htmlToInsert += `<rect x="1.5" y="1.5" rx="2" ry="2" width="19" height="19" stroke="#F2F2F2" stroke-width="1px" fill="#` + colourToHex(colour) + `" opacity="1"/>`
        } else {
            htmlToInsert += `<rect x="1" y="1" rx="2" ry="2" width="20" height="20" stroke="black" stroke-width="0px" fill="#` + colourToHex(colour) + `" opacity="1"/>`
        }
    }
    if (colour == -1){
        //Transparent (T)
        htmlToInsert += `<line x1="8" y1="7" x2="14" y2="7" stroke="black" stroke-width="1px" />`
        htmlToInsert += `<line x1="11" y1="7" x2="11" y2="15" stroke="black" stroke-width="1px" />`
    }
    if (colour < -1 || colour > 63){
        //Unknown (X)
        htmlToInsert += `<line x1="8" y1="7" x2="14" y2="15" stroke="red" stroke-width="1px" />`
        htmlToInsert += `<line x1="8" y1="15" x2="14" y2="7" stroke="red" stroke-width="1px" />`
    }
    return htmlToInsert;
}

function generatePixelElement(id) {
    const pixel = document.createElementNS('http://www.w3.org/2000/svg','svg');
    pixel.classList.add("m-0");
    pixel.classList.add("p-0");
    pixel.setAttribute("width", "22");
    pixel.setAttribute("height", "22");
    pixel.setAttribute("style", "cursor: pointer;");
    pixel.setAttribute("id", "pixel-" + String(id));
    //pixel.classList.add('pixel');
    let id_arg = id;
    pixel.addEventListener('click', () => pixelOnClick(id_arg));
    pixel.insertAdjacentHTML('beforeend', generatePixelSvgContent(id));
    return pixel;
}

function generatePalColourSvgContent(colour){
    let htmlToInsert = "";
    htmlToInsert += `<rect x="2" y="2" rx="2" ry="2" width="28" height="28" stroke="black" stroke-width="1px" fill="#` + colourToHex(colour) + `" opacity="1"/>`
    if (colour == -1){
        //Transparent (T)
        htmlToInsert += `<line x1="8" y1="9" x2="24" y2="9" stroke="black" stroke-width="1px" />`
        htmlToInsert += `<line x1="16" y1="9" x2="16" y2="24" stroke="black" stroke-width="1px" />`
    }
    if (colour < -1 || colour > 63){
        //Unknown (X)
        htmlToInsert += `<line x1="8" y1="7" x2="14" y2="15" stroke="red" stroke-width="1px" />`
        htmlToInsert += `<line x1="8" y1="15" x2="14" y2="7" stroke="red" stroke-width="1px" />`
    }
    return htmlToInsert;
}

function generatePalColourElement(colour) {
    const palColour = document.createElementNS('http://www.w3.org/2000/svg','svg');
    palColour.classList.add("m-0");
    palColour.classList.add("p-0");
    palColour.setAttribute("width", "32");
    palColour.setAttribute("height", "32");
    palColour.setAttribute("style", "cursor: pointer;");
    //palColour.setAttribute("id", "pal_colour-" + String(colour));
    //pixel.classList.add('pixel');
    let id_arg = colour;
    palColour.addEventListener('click', () => palColourOnClick(id_arg));
    palColour.insertAdjacentHTML('beforeend', generatePalColourSvgContent(colour));
    return palColour;
}

function populatePixelMap() {
    // Generate the 32x32 pixel grid
    for (let rowCount = -1; rowCount < mapHeight; rowCount++) {
        //begnning of row
        const row = document.createElement('div');
        row.classList.add('d-flex');
        row.classList.add('flex-row');
        row.classList.add('m-0');
        row.classList.add('p-0');
        if (rowCount == -1) {
            row.appendChild(generateIDPixelElement(''));
            for (let colCount = 0; colCount < mapWidth; colCount++) {
                //Column element
                row.appendChild(generateIDPixelElement(colCount.toString()));
            }
        } else {
            row.appendChild(generateIDPixelElement(rowCount.toString()));
            for (let colCount = 0; colCount < mapWidth; colCount++) {
                //Column element
                let i = colCount + mapWidth * rowCount;
                row.appendChild(generatePixelElement(colCount + rowCount * mapWidth));
            }
        }
        //End of row
        pixelMapElem.appendChild(row);
    }
}


function populatePalette() {
    // Generate the colour palette
    for (let rowCount = 0; rowCount < Math.ceil((colourCount)/paletteWidth); rowCount++) {
        //Begnning of row
        const row = document.createElement('div');
        row.classList.add('d-flex');
        row.classList.add('flex-row');
        row.classList.add('m-0');
        row.classList.add('p-0');
        for (let colCount = 0; colCount < paletteWidth; colCount++) {
            //Column element
            row.appendChild(generatePalColourElement(colCount + rowCount * paletteWidth));
        }
        //End of row
        palette.appendChild(row);
    }
    // Generate transparent row
    const row = document.createElement('div');
    row.classList.add('d-flex');
    row.classList.add('flex-row');
    row.classList.add('m-0');
    row.classList.add('p-0');
    row.appendChild(generatePalColourElement(-1));
    palette.appendChild(row);
}

function generateMemoryContent() {
    let text = "";
    for (let i = 0; i < colours.length; i++) {
        text += colourToBinary(colours[i]);
        if (i < colours.length -1){
            text += "\r\n";
        }
    }
    return text;
}

function updateMemoryContent() {
    memoryContentElem.value = generateMemoryContent();
}

const previewScale = 3;
function updatePreview() {
    const previewImage = document.createElementNS('http://www.w3.org/2000/svg','svg');
    previewImage.classList.add("m-0");
    previewImage.classList.add("p-0");
    previewImage.setAttribute("width", String(previewScale * mapWidth));
    previewImage.setAttribute("height", String(previewScale * mapHeight));
    let htmlToInsert = "";
    for (let rowCount = 0; rowCount < mapHeight; rowCount++) {
        for (let colCount = 0; colCount < mapWidth; colCount++) {
                htmlToInsert += `<rect x="` + String(previewScale * colCount) +
                `" y="` + String(previewScale * rowCount) +
                `" rx="0" ry="0" width="` + String(previewScale) +
                `" height="` + String(previewScale) +
                `" stroke="black" stroke-width="0px" fill="#` + colourToHex(colours[rowCount * mapWidth + colCount]) +
                `" opacity="1"/>`;
        }
    }
    previewImage.insertAdjacentHTML('beforeend', htmlToInsert);
    while (previewElem.firstChild) {
        previewElem.removeChild(previewElem.lastChild);
      }
    previewElem.appendChild(previewImage);
}

memoryContentElem.addEventListener('change', () => memoryContentOnChange());

function memoryContentOnChange(){
    //console.log("Memory content changed by user.");
    //updateMemoryContent();
    let error = updateAllFromMemoryString(memoryContentElem.value);
    if (error){
        showToast("The raw memory content contains mistakes.", true);
    }
}

function dowloadMemFile(){
    var content = generateMemoryContent();
    const link = document.createElement("a");
    const file = new Blob([content], { type: 'text/plain' });
    link.href = URL.createObjectURL(file);
    link.download = fileNameElem.value + ".mem";
    link.click();
    URL.revokeObjectURL(link.href);
    for (let i = 0; i < mapWidth * mapHeight; i++) {
        if (colours[i] < -1 || colours[i] > 63){
            showToast("Saving a file that contains undefined pixels.", true);
            break;
        }
    }
}

function updateAllFromMemoryString(content){
    let error = false;
    lines = content.split('\n');
    let i = 0;
    for (; i < lines.length && i < mapWidth * mapHeight; i++){
        lines[i] = lines[i].replaceAll('\r', '');
        colours[i] = binaryToColour(lines[i]);
       if (colours[i] < -1 || colours[i] > 63){
           error = true;
       }
    }
    for (; i < mapWidth * mapHeight; i++){
        colours[i] = binaryToColour("1111111");
        error = true;
    }
    for (let i = 0; i < mapWidth * mapHeight; i++) {
        selected[i] = (i == 0);
        let pixel = document.getElementById("pixel-" + String(i));
        pixel.innerHTML = generatePixelSvgContent(i);
    }
    var lastPixelSelected = 0;
    updatePreview();
    updateMemoryContent();
    return error;
}

uploadButtonElem.onchange = function() {
    if(uploadButtonElem.files.length == 0 || typeof uploadButtonElem.files === 'undefined'){
        //console.log("File input not valid or empty or more than 1.");
        showToast("Loading file failed. File input not valid, empty, or including multiple files.", true);
        return;
    }
    file = uploadButtonElem.files[0];

    if (!file || typeof file === 'undefined') {
        //console.log("Cannot load files.");
        showToast("Loading file failed. File is NULL or undefined.", true);
        return;
    }

    const reader = new FileReader();
    reader.onload = function(e) {
      const content = e.target.result;
      //console.log(content); // Print content to console
      let error = updateAllFromMemoryString(content);
      if (error){
          showToast("The uploaded file contains mistakes or it is too long or too short.", true);
      }
    };
    reader.readAsText(file);
    fileNameElem.value = (file.name).slice(0, file.name.length - 4);
}

function clearAll(){
    for (let i = 0; i < mapWidth * mapHeight; i++) {
        colours[i] = -1;
        selected[i] = (i == 0);
        let pixel = document.getElementById("pixel-" + String(i));
        pixel.innerHTML = generatePixelSvgContent(i);
    }
    var lastPixelSelected = 0;
    updatePreview();
    updateMemoryContent();
    fileNameElem.value = "tile";
}

downloadButtonElem.addEventListener('click', () => dowloadMemFile());
clearButtonElem.addEventListener('click', () => clearAll());

function copyToClipboard(){
  navigator.clipboard.writeText(generateMemoryContent());
  showToast("Memory content copied to the clipboard.", false);
}

copyElem.addEventListener('click', () => copyToClipboard());

function fileNameOnChange(){
    if (fileNameElem.value.length == 0){
        fileNameElem.value = "tile";
        showToast("The file name cannot be empty. Using default name 'tile.mem'.", true);
        return;
    }
    let pattern = /^[ \dA-Za-z_-]+$/;
    if (!pattern.test(fileNameElem.value)){
        fileNameElem.value = "tile";
        showToast("The file name cannot contain charachers other than letters, numbers, spaces, '-', and '_'. Using default name 'tile.mem'.", true);
        return;
    }
    fileNameElem.value = fileNameElem.value.trim();
    if (fileNameElem.value.includes(" ")){
        showToast("It is not recommended to have spaces in the file name.", false);
    }
}

fileNameElem.addEventListener('change', () => fileNameOnChange());

function showToast(message, red) {
    if (red){
        toastBody.innerHTML = `<p class="fw-bold text-danger m-0 p-0">` + message + `</p>`;
    } else {
        toastBody.innerHTML = `<p class="fw-bold m-0 p-0">` + message + `</p>`;
    }
    var toastElList = [].slice.call(document.querySelectorAll('.toast'))
    var toastList = toastElList.map(function(toastEl) {
        return new bootstrap.Toast(toastEl)
    })
    toastList.forEach(toast => toast.show())
}

window.onload = function() {
    populatePixelMap();
    populatePalette();
    updatePreview();
    updateMemoryContent();
    fileNameElem.value = "tile";
};

//SCOPE
})();