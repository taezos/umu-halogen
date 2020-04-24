function main ()  {
  require("../output/Main").main();
}

if ( module.hot ) {
  module.hot.dispose(function () {
    console.log("[INFO]: Dispose...");
    document.body.innerHTML = "";
  });

  module.hot.accept(function () {
    console.log("[INFO]: Accept...");
    document.body.innerHTML = "";
    main();
  });
}

main();
