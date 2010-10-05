import demo__POA

from Acspy.Servants.ContainerServices  import ContainerServices
from Acspy.Servants.ComponentLifecycle import ComponentLifecycle
from Acspy.Servants.ACSComponent       import ACSComponent

class ComponentGetterImpl(demo__POA.ComponentGetter,
                          ACSComponent,
                          ContainerServices,
                          ComponentLifecycle):

	def __init__(self):
		ACSComponent.__init__(self)
		ComponentLifecycle.__init__(self)
		ContainerServices.__init__(self)

	def getOtherComponent(self):
		self.getComponent("COMP_TO_GET")
		self.getLogger().logInfo("Got component without problems :)");
