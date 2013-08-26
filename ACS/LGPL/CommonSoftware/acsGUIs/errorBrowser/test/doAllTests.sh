#!/bin/sh
acsStartJava junit.textui.TestRunner cl.utfsm.acs.types.TypesSuite
acsStartJava -endorsed -DACS.acsroot="`pwd`/xml" -Dtest.xmldirs="`pwd`/xml" junit.textui.TestRunner cl.utfsm.acs.xml.XmlSuite
acsStartJava -endorsed -DACS.acsroot=$ACSROOT -Dtest.xmldirs="`pwd`/xml" junit.textui.TestRunner cl.utfsm.acs.ebe.EbeSuite
