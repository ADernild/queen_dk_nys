console.log("Welcome to the true console log experience.");
function getTopics() {
  console.log("Ran getTopics");
  let arr = [];
  /*let topics = document.getElementsByClassName("dot");
  //console.log(topics);
  for (let i = 0; i < topics.length; i++) {
    topics[i].addEventListener("click", function(){*/
  let terms = document.getElementsByClassName("terms");
  for(let i = 0; i < terms.length; i++){
    arr.push(terms[i].textContent);
    //console.log(terms[i])
  }
  //console.log(arr);
  /*  });
  }*/
  return(arr);
}
getTopics();

//var myVariable = 1; //do something
//Shiny.onInputChange("variableNameToWriteInServer.R", myVariable)

Shiny.addCustomMessageHandler('getTopics', function(mess) {
  tops = getTopics();
  
  Shiny.setInputValue("tippertoppertopicspopper", tops.toString())
  //alert(tops);
  //alert(mess.ok)
})
