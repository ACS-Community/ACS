#!/bin/bash
acsStartContainer -cpp bilboContainer >& ./bilboContainer.log &
acsStartContainer -cpp bilboContainer2 >& ./bilboContainer2.log &
