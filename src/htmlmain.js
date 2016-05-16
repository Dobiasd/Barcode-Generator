function init() {
  var mainDiv = document.getElementById('main');
  elmContent = Elm.BarcodeGenerator.embed(mainDiv);
  elmContent.ports.saveImagePort.subscribe(saveImage);
}

function saveImage(barcode)
{
  var canvasPromise = html2canvas(document.getElementById('barcodeDiv'), {
    letterRendering: true,
  }).then(function(canvas) {
    var link = document.createElement('a');
    link.href = canvas.toDataURL('image/png');
    link.download = barcode + '.png';
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
  });
}