<!--
	function makeArray() 
	{
		var args = makeArray.arguments;
		for (var i = 0; i < args.length; i++) 
			this[i] = args[i];
		this.length = args.length;
	}
		
	function getString() 
	{
	   var months= new makeArray("January","February","March","April","May","June","July","August","September","October","November","December");	
      var timeval= new Date(document.lastModified);
		var base= new Date(0);
		var skew= base.getTime();
		if (skew > 0)
			timeval.setTime(timeval.getTime() - skew);
	   var minutes= timeval.getMinutes();
	   if(timeval.getMinutes() < 10)
	      minutes= "0" + timeval.getMinutes();
	   var hours= timeval.getHours();
	   if(timeval.getHours() < 10)
	      hours= "0" + timeval.getHours();
	   
	  if (navigator.appName == "Netscape") 
	     return timeval.getDate() + " " + months[timeval.getMonth()] + " " + (1900 + timeval.getYear()) + " - " + hours + ":" + minutes;
	  else   
	     return timeval.getDate() + " " + months[timeval.getMonth()] + " " + timeval.getYear() + " - " + hours + ":" + minutes;	  			    
	}
	
   function updateTimeStamp() 
   {
      document.write(getString());
   }
   updateTimeStamp();
// -->