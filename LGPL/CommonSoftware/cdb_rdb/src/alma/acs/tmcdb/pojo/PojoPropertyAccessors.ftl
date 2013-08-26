<#-- // Property accessors -->
<#foreach property in pojo.getAllPropertiesIterator()>
<#if pojo.getMetaAttribAsBool(property, "gen-property", true)>
 <#if pojo.hasFieldJavaDoc(property)>    
    /**       
     * ${pojo.getFieldJavaDoc(property, 4)}
     */
</#if>
    <#include "GetPropertyAnnotation.ftl"/>
    ${pojo.getPropertyGetModifiers(property)} ${pojo.getJavaTypeName(property, jdk5)} ${pojo.getGetterSignature(property)}() {
        return this.${property.name};
    }
    
    ${pojo.getPropertySetModifiers(property)} void set${pojo.getPropertyName(property)}(${pojo.getJavaTypeName(property, jdk5)} ${property.name}) {    
    <#if pojo.isComponent() || c2h.isCollection(property) >
    	this.${property.name} = ${property.name};
    <#else>
        if( propertyChangeSupport != null )
            propertyChangeSupport.firePropertyChange("${property.name}", this.${property.name}, this.${property.name} = ${property.name});
        else
            this.${property.name} = ${property.name};
    </#if>
    }

<#if c2h.isCollection(property)>
<#assign remoteClassName=pojo.importType(property.getValue().getElement().getReferencedEntityName())>
	${pojo.getPropertySetModifiers(property)} void add${pojo.getPropertyName(property)}(${pojo.getJavaTypeName(property, jdk5)} elements) {
		if( this.${property.name} != null )
			for(${pojo.importType("java.util.Iterator")}<${remoteClassName}> it = elements.iterator(); it.hasNext(); )
				add${remoteClassName}To${pojo.getPropertyName(property)}((${remoteClassName})it.next());
	}

	${pojo.getPropertySetModifiers(property)} void add${remoteClassName}To${pojo.getPropertyName(property)}(${remoteClassName} element) {
		if( !this.${property.name}.contains(element) ) {
			this.${property.name}.add(element);
			<#if c2h.isManyToMany(property)>
			<#--
				Will have to comment it out in the meanwhile,
				since I don't see a clear solution to get the referenced property name
				out of the property object.
				Closest solution: private EntityPOJOClass.getOneToManyMappedBy(cfg,property)

			element.add${pojo.getDeclarationName()}To---SOMETHING---(this);
			-->
			<#else>
			<#--
				Will have to comment it out in the meanwhile,
				since I don't see a clear solution to get the referenced property name
				out of the property object.
				Closest solution: private EntityPOJOClass.getOneToManyMappedBy(cfg,property)

			element.set${pojo.getDeclarationName()}(this);
			-->
			</#if>
		}
	}

</#if>
</#if>
</#foreach>