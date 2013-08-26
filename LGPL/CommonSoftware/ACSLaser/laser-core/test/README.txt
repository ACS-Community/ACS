The ACS Alarm System Test Scenario
==================================

Author: Klemen Zagar, Cosylab

History:

  2005-06-13  KZ  Created

Description
-----------

In this test scenario, there are two components: ALARM_SOURCE_A and
ALARM_SOURCE_B (see MACI/Components). These components are sources of
alarms of type ACS_ALARM_SOURCE, which is the one and only alarm source
type (see Alarms/SourceDefinitions).

All possible fault states (alarms) are listed in CDB record Alarms.
The components ALARM_SOURCE_A and ALARM_SOURCE_B define 4 fault-states
(alarm definitions) each:

  * fault with code 1 (say, underflow) on property 'current'
  * fault with code 2 (say, overflow) on property 'current'
  * fault with code 1 on the component itself
  * fault with code 2 on the component itself

(See records Alarms/AlarmDefinitions/ALARM_SOURCE*.)

Reduction is defined as follows:

 * fault-states of 'current' are children of fault-state of the component
 * if at least two fault-states at the component-level are active, the
   abstract fault state (not related to any particular component) called
   "ALARM_SOURCES" is activated

These reduction rules are defined in Alarms/ReductionDefinitions.

Also, each alarm belongs to zero or more categories. In this example,
only categories ROOT, ROOT:TEST and ROOT:CURRENT are defined.
The assignment of alarms to categories is specified in CDB record
Alarms/AlarmCategoryDefinitions.

___oOo___
