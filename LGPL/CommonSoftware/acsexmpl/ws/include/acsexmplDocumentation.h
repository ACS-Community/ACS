///////////////////////////////////////////////////////////////////////////////////
//MAIN ENTRY FOR ALL ACSEXMPL DOCUMENTATION...just a table of links!
/** @defgroup ACSEXMPLDOC ACS C++ Examples Documentation
 *  @{
 *  @htmlonly
<hr>
<h1>Component Examples</h1>
<table width="100%" border="1" cellspacing="2" cellpadding="2">
  <tbody>
    <tr>
      <td><h2>Example</h2></td>
      <td valign="top"><h2>Brief Description</h2></td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLAMSSEQDOC.html">AmsSeq</a></td>
      <td valign="top">Antenna Mount System Pointing Model</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLBUILDINGDOC.html">Building</a></td>
      <td valign="top">Implements an hierarchical device using <a href="group__ACSEXMPLDOORDOC.html">Door</a>.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCALENDARDOC.html">Calendar</a></td>
      <td valign="top">Simulates the behavior of a calendar using enums and pattern properties.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLDOORDOC.html">Door</a></td>
      <td valign="top">Door is a simple component that utilizes just about every ACS API.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLFRIDGEDOC.html">Fridge</a></td>
      <td valign="top">FridgeControl shows BACI threads as well as a notification channel supplier.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLHWDOC.html">Hello World</a></td>
      <td valign="top">ACS Hello World example.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLLAMPDOC.html">Lamp</a></td>
      <td valign="top">Simple component with asynchronous methods.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLMOUNTDOC.html">Mount</a></td>
      <td valign="top">Simulates the behavior of an antenna interface.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLPSDOC.html">Power Supply</a></td>
      <td valign="top">Simulates the behavior of a power supply and overwrites an ACS property.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLRPSDOC.html">Ramped Power Supply</a></td>
      <td valign="top">Simulates the behavior of a ramped power supply by inheriting from Power Supply's IDL interface and C++ implementation.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLSLOWMOUNTDOC.html">Slow Mount</a></td>
      <td valign="top">Similar to Mount, but simulate the movements of the antenna in a 30 secs time interval.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLLAMPWHEELDOC.html">Lamp Wheel</a></td>
      <td valign="top">Simulates the behaviour of a generic lamp wheel, using an asynchronous method to rotate it.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLFILTERWHEELDOC.html">Filter Wheel</a></td>
      <td valign="top">Simulates the behaviour of a generic filter wheel.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLERRORCOMPDOC.html">Error Component</a></td>
      <td valign="top">Simple Component that throws few exceptions and completions to show the use.</td>
    </tr>
  </tbody>
</table>
<br>
<h1>Client Examples</h1>
<table width="100%" border="1" cellspacing="2" cellpadding="2">
  <tbody>
    <tr>
      <td><h2>Example</h2></td>
      <td valign="top"><h2>Brief Description</h2></td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTHWDOC.html">Client Hello World</a></td>
      <td valign="top">A very simple client which interacts with the <a href="group__ACSEXMPLHWDOC.html">Hello World</a> component.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTLISTCOMPONENTSDOC.html">Client List Components</a></td>
      <td valign="top">Lists all static components known to manager.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTDOC.html">Client</a></td>
      <td valign="top">Logs into manager and manipulates <a href="group__ACSEXMPLMOUNTDOC.html">mount</a> components. Invokes an asynchronous method.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTWAVEDOC.html">Client Wave</a></td>
      <td valign="top">Within a BACI thread, a <a href="group__ACSEXMPLMOUNTDOC.html">mount</a> component's positions follow a sine wave.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTAMSSEQDOC.html">Client AmsSeq</a></td>
      <td valign="top">Obtains a reference to an <a href="group__ACSEXMPLAMSSEQDOC.html">AmsSeq</a> component's pointing model, prints it, and then changes it.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTFRIDGECMDDOC.html">Client Fridge Command</a></td>
      <td valign="top">Commands specified from the command-line are invoked on a <a href="group__ACSEXMPLFRIDGEDOC.html">fridge</a> component.</td>
    </tr> 
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTFRIDGEDOC.html">Client Fridge</a></td>
      <td valign="top">Creates a BACI monitor using callbacks (running for 20 seconds) on the temperature property of a <a href="group__ACSEXMPLFRIDGEDOC.html">fridge</a> component.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTFRIDGENCDOC.html">Client Fridge Channel Event Consumer</a></td>
      <td valign="top">Shows the implementation of an event channel consumer consuming events from a <a href="group__ACSEXMPLFRIDGEDOC.html">fridge</a> component.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTALARMTHREADDOC.html">Client Alarm Thread</a></td>
      <td valign="top">Creates a BACI alarm set to go off when the value of a <a href="group__ACSEXMPLPSDOC.html">power supply</a>'s read-only double property goes out of range.  This value "goes out of range" within a BACI thread.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTDYNAMICCOMPONENTDOC.html">Client Dynamic Component</a></td>
      <td valign="top">Simple example retrieves a dynamic <a href="group__ACSEXMPLHWDOC.html">Hello World</a> component from manager.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTCOMPONENTIORDOC.html">Client Component IOR</a></td>
      <td valign="top">Prints out the stringified IOR of a component specified from the command-line.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTASYNCCALLSDOC.html">Async Calls</a></td>
      <td valign="top">Client that uses asynchronous calls.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCLIENTERROROCOMPDOC.html">Client Error Component</a></td>
      <td valign="top">Client that uses the Error Component to catch exception/completion and their handling.</td>
    </tr>
  </tbody>
</table>
<br>
<h1>Miscellaneous Examples</h1>
<table width="100%" border="1" cellspacing="2" cellpadding="2">
  <tbody>
    <tr>
      <td><h2>Example</h2></td>
      <td valign="top"><h2>Brief Description</h2></td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLCALLBACKSDOC.html">BACI Callback and Alarm Implementations</a></td>
      <td valign="top">The C++ implementations of various BACI IDL interfaces is shown here.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLLONGDEVIODOC.html">DevIO Implementation</a></td>
      <td valign="top">The implementation of a DevIO subclass is shown here.</td>
    </tr>
    <tr>
      <td valign="top"><a href="group__ACSEXMPLPSCURRENTDOC.html">Overriding BACI Properties</a></td>
      <td valign="top">The BACI property of a <a href="group__ACSEXMPLPSDOC.html">Power Supply</a> is overriden in this example.</td>
    </tr>
  </tbody>
</table>
<br>
<hr>
<br>
<br>
<br>
<br>
<br>
<br>
<h2>TODO List:</h2>
<ul>
  <li>Documentation missing or outdated in a few clients still.</li>
  <li>Rename files in ../test/ref_WS/*</li>
  <li>ContainerServices NULL pointer in building example.</li>
  <li>Substate property in the door example should be an enumeration!</li>
  <li>yearAttributes property in Calendar should be a RWlong!</li>
  <li>FridgeControl implementation should contain a PowerSupply and Door</li>
  <li>badMethod of HelloWorld IDL interface needs to be upgraded to the new ACS Error System.</li>
  <li>rampingStatus property should in RPS should be an enum.</li>
  <li>RPS interface should be moved into IDL for PS???</li>
  <li>AmsSeq and Mount should be moved into the same IDL</li>
  <li>defines in acsexmplPowerSupplyImpl.h should be moved to the class as public const static integers to comply w/ coding standards.</li>
  <li>defines in acsexmplRampedPowerSupplyImpl.h should be moved to the class as public const static integers to comply w/ coding standards.</li>
</ul>
  @endhtmlonly
 * @}
*/
///////////////////////////////////////////////////////////////////////////////////
