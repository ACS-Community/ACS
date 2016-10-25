<#if ejb3>
<#if pojo.hasIdentifierProperty()>
<#if property.equals(clazz.identifierProperty)>
<#if pojo.hasMetaAttribute("oracle-sequence") >
    @${pojo.importType("javax.persistence.Id")} @${pojo.importType("javax.persistence.GeneratedValue")}(generator="generator")
    @${pojo.importType("org.hibernate.annotations.GenericGenerator")}(name="generator", strategy="native",
       parameters = {@${pojo.importType("org.hibernate.annotations.Parameter")}(name="sequence", value="${pojo.getMetaAsString("oracle-sequence")}")}
	)
<#else>
 ${pojo.generateAnnIdGenerator()}
</#if>
</#if>
</#if>

<#if c2h.isOneToOne(property)>
${pojo.generateOneToOneAnnotation(property, cfg)}
<#elseif c2h.isManyToOne(property)>
${pojo.generateManyToOneAnnotation(property)}
<#--TODO support optional and targetEntity-->    
${pojo.generateJoinColumnsAnnotation(property, cfg).replaceFirst("=\"", "=\"`").replaceAll("\",", "`\",").replaceAll("\"\\)","`\")")}
<#elseif c2h.isCollection(property)>
${pojo.generateCollectionAnnotation(property, cfg)}
<#else>
${pojo.generateBasicAnnotation(property)}
${pojo.generateAnnColumnAnnotation(property).replaceFirst("=\"", "=\"`").replaceAll("\",", "`\",").replaceAll("\"\\)","`\")")}
<#-- Added by ACS to support the @Type annotation -->
<#if pojo.getMetaAttribAsBool(property, "isXmlClobType", false) >
    @${pojo.importType("org.hibernate.annotations.Type")}(type="xmltype")
</#if>
<#assign name = pojo.getPropertyName(property)?lower_case>
<#if pojo.getMetaAsString("enum-types")?contains(name+"|")>
	@${pojo.importType("org.hibernate.annotations.Type")}(type="${pojo.getJavaTypeName(property, jdk5)}")
</#if>
</#if>
</#if>
