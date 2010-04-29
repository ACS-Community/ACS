<#if ejb3?if_exists>
<#if pojo.isComponent()>
@${pojo.importType("javax.persistence.Embeddable")}
<#else>
@${pojo.importType("javax.persistence.Entity")}
@${pojo.importType("javax.persistence.Table")}(name="${clazz.table.name}"
<#-- We are commenting these two attributes since we need them to NOT be present in our generated pojos,
     mainly because of the Oracle DB not being complaint with the PUBLIC schema
     that is auto-generated when importing the SQL code from HSQLDB
<#if clazz.table.schema?exists>
    ,schema="${clazz.table.schema}"
</#if><#if clazz.table.catalog?exists>
    ,catalog="${clazz.table.catalog}"
</#if>
-->
<#assign uniqueConstraint=pojo.generateAnnTableUniqueConstraint()>
<#if uniqueConstraint?has_content>
    , uniqueConstraints = ${uniqueConstraint} 
</#if>)
<#if pojo.getMetaAttribAsBool(pojo.getDecoratedObject(), "isSuperClass", false)>
@${pojo.importType("javax.persistence.Inheritance")}(strategy=${pojo.importType("javax.persistence.InheritanceType")}.JOINED)
</#if>
<#if pojo.getMetaAttribAsBool(pojo.getDecoratedObject(), "hasXmlClobType", false)>
@${pojo.importType("org.hibernate.annotations.TypeDef")}(name="xmltype", typeClass=${pojo.importType("alma.hibernate.util.HibernateXmlType")}.class)
</#if>
</#if>
</#if>