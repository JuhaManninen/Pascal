<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">


<!-- parameter is package-id of the package that is to be generated -->
<xsl:param name="package_id">xmi_6</xsl:param>


<xsl:key name="generalization" match="//Foundation.Core.Generalization" use="@xmi.id"/>

<xsl:key name="classifier"
  match="//Foundation.Core.Class|//Foundation.Core.Interface|//Foundation.Core.DataType"
  use="@xmi.id"/>

<xsl:key name="abstraction" match="//Foundation.Core.Abstraction" use="@xmi.id"/>


<!-- Root -->
<xsl:template match="/">

<html>

  <link href="styles.css" rel="stylesheet" type="text/css"/>

  <head> 
  </head>
  

  <body class="body">
    <xsl:apply-templates select="//Model_Management.Package[@xmi.id=$package_id]"/>
  </body>

</html>


</xsl:template>
<!-- End Root -->




<xsl:template match="Model_Management.Package">
  <table width="100%">
    <tr>
      <td>
        <span class="p_name">Package: <xsl:value-of select="Foundation.Core.ModelElement.name"/> </span>
      </td>
      <td class="t_cell">
        <a href="p_{@xmi.id}_diagram.html">Diagram</a>
      </td>
      <td class="t_cell">
        <a href="overview.html">Back to overview</a>
      </td>
    </tr>
  </table>
  <p/>
  
  <!-- Classes -->
  <xsl:apply-templates select="Foundation.Core.Class[@xmi.id]">
    <xsl:sort select="Foundation.Core.ModelElement.name"/>
  </xsl:apply-templates>
  
  <!-- Interfaces -->
  <xsl:apply-templates select="Foundation.Core.Interface[@xmi.id]">
    <xsl:sort select="Foundation.Core.ModelElement.name"/>
  </xsl:apply-templates>
  
</xsl:template>




<!-- CLASS -->
<xsl:template match="Foundation.Core.Class">
  <xsl:variable name="name" select="Foundation.Core.ModelElement.name"/>
  <a name="{@xmi.id}"> </a>

  <table width="500" cellspacing="1" cellpadding="2" border="0">
    <tr>
      <td class="ClassTableHeader" width="200">
        <b><xsl:value-of select="$name"/></b>
      </td>
      <td class="ClassTableHeader" width="150">
        class
      </td>
    </tr>
  </table>

  <xsl:call-template name="inherits_from"/>
  <xsl:call-template name="implements"/>
  <xsl:call-template name="subclasses"/>
  <xsl:call-template name="attributes"/>
  <xsl:call-template name="operations"/>
  
  <br/><br/>
</xsl:template>
<!-- End CLASS -->



<!-- INTERFACE -->
<xsl:template match="Foundation.Core.Interface">
  <xsl:variable name="name" select="Foundation.Core.ModelElement.name"/>
  <a name="{@xmi.id}"> </a>

  <table width="500" cellspacing="1" cellpadding="2" border="0">
    <tr>
      <td class="ClassTableHeader" width="200">
        <b><xsl:value-of select="$name"/></b>
      </td>
      <td class="ClassTableHeader" width="150">
        interface
      </td>
    </tr>
  </table>

  <xsl:call-template name="inherits_from"/>
  <xsl:call-template name="implements"/>
  <xsl:call-template name="realizations"/>
  <xsl:call-template name="attributes"/>
  <xsl:call-template name="operations"/>
  
  <br/><br/>
</xsl:template>
<!-- End INTERFACE -->






<xsl:template name="inherits_from">
  <xsl:variable name="generalizations"         
   select="Foundation.Core.GeneralizableElement.generalization/Foundation.Core.Generalization"/>

  <xsl:if test="count($generalizations) > 0">    

    Inherits from: 

    <xsl:for-each select="$generalizations">

     <xsl:variable name="generalization"
      select="key('generalization', ./@xmi.idref)" />

     <xsl:variable name="target"
      select="$generalization/Foundation.Core.Generalization.parent/*/@xmi.idref" />

     <xsl:call-template name="name_with_link">
       <xsl:with-param name="node" select="$target"/>
     </xsl:call-template>

     <xsl:if test="position() != last()">
       <xsl:text>,  </xsl:text>
     </xsl:if>

    </xsl:for-each>
    <br/>
  </xsl:if>
</xsl:template>



<!-- List of interfaces that a class implements -->
<xsl:template name="implements">
  <xsl:variable name="specifications" 
       select="Foundation.Core.ModelElement.clientDependency/
               Foundation.Core.Abstraction"/>

  <xsl:if test="count($specifications) > 0">

    Implements: 

    <xsl:for-each select="$specifications">

      <xsl:variable name="abstraction"  select="key('abstraction', ./@xmi.idref)" />
      <xsl:variable name="target" select="$abstraction/Foundation.Core.Dependency.supplier/*/@xmi.idref" />

      <xsl:call-template name="name_with_link">
        <xsl:with-param name="node" select="$target"/>
      </xsl:call-template>

      <xsl:if test="position() != last()">
        <xsl:text>,  </xsl:text>
      </xsl:if>

    </xsl:for-each>
    <br/>
  </xsl:if>
</xsl:template> 



<!-- List of classes that implements an interface -->
<xsl:template name="realizations">

  <xsl:variable name="realizations" 
       select="Foundation.Core.ModelElement.supplierDependency/
               Foundation.Core.Abstraction"/>

  <xsl:if test="count($realizations) > 0">

    Implementing classes: 

    <xsl:for-each select="$realizations">

      <xsl:variable name="abstraction"  select="key('abstraction', ./@xmi.idref)" />
      <xsl:variable name="target" select="$abstraction/Foundation.Core.Dependency.client/*/@xmi.idref" />

      <xsl:call-template name="name_with_link">
        <xsl:with-param name="node" select="$target"/>
      </xsl:call-template>

      <xsl:if test="position() != last()">
        <xsl:text>,  </xsl:text>
      </xsl:if>

    </xsl:for-each>
    <br/>

  </xsl:if>  
        
</xsl:template>





<xsl:template name="subclasses">
  <xsl:variable name="specializations"
          select="Foundation.Core.GeneralizableElement.specialization/
          Foundation.Core.Generalization"/>

  <xsl:if test="count($specializations) > 0">

    Subclasses: 

    <xsl:for-each select="$specializations">

     <xsl:variable name="generalization"
      select="key('generalization', ./@xmi.idref)" />

     <xsl:variable name="target" select="$generalization/Foundation.Core.Generalization.child/*/@xmi.idref" />

     <xsl:call-template name="name_with_link">
       <xsl:with-param name="node" select="$target"/>
     </xsl:call-template>

     <xsl:if test="position() != last()">
       <xsl:text>,  </xsl:text>
     </xsl:if>

    </xsl:for-each>
    <br/>
  </xsl:if>
</xsl:template>




<xsl:template name="attributes">
  <xsl:variable name="attr"
   select="Foundation.Core.Classifier.feature/Foundation.Core.Attribute"/>

  <xsl:if test="count($attr) > 0">
    <b>Attributes</b>

    <table width="100%" cellpadding="3" cellspacing="0" border="1">
      <tr>
        <td class="tableheader">Name</td>
        <td class="tableheader">Type</td>
        <td class="tableheader">Visibility</td>
      </tr>

      <xsl:apply-templates select="Foundation.Core.Classifier.feature/Foundation.Core.Attribute">
        <xsl:sort select="Foundation.Core.ModelElement.visibility/@xmi.value" />
      </xsl:apply-templates>
      
    </table>
  </xsl:if>
</xsl:template>




<xsl:template match="Foundation.Core.Attribute">
  <xsl:variable name="node"
    select='Foundation.Core.StructuralFeature.type/*/@xmi.idref'/>

  <tr>
    <td class="t_cell">
      <xsl:value-of select="Foundation.Core.ModelElement.name"/>
    </td>
    <td class="t_cell">
      <xsl:call-template name="name_with_link">
        <xsl:with-param name="node" select="$node"/>
      </xsl:call-template>
      &#160;
    </td>
    <td class="t_cell">
      <xsl:value-of select="Foundation.Core.ModelElement.visibility/@xmi.value"/>
    </td>
  </tr>
</xsl:template>




<xsl:template name="operations">
  <xsl:variable name="ops"
   select="Foundation.Core.Classifier.feature/Foundation.Core.Operation"/>

  <xsl:if test="count($ops) > 0">
    <b>Operations</b>

    <table width="100%" cellpadding="3" cellspacing="0" border="1">
      <tr>
        <td class="tableheader">Name</td>
        <td class="tableheader">Parameters</td>
        <td class="tableheader">Returns</td>
        <td class="tableheader">Visibility</td>
      </tr>

      <xsl:apply-templates select="Foundation.Core.Classifier.feature/Foundation.Core.Operation">
        <xsl:sort select="Foundation.Core.ModelElement.visibility/@xmi.value" />
      </xsl:apply-templates>
      
    </table>

  </xsl:if>
</xsl:template>



<!-- 160 is non-breaking space -->
<xsl:template match="Foundation.Core.Operation">
  <xsl:variable name="return"
   select="Foundation.Core.BehavioralFeature.parameter/Foundation.Core.Parameter[Foundation.Core.Parameter.kind/@xmi.value='return']" />

  <xsl:variable name="return_type"
   select="$return/Foundation.Core.Parameter.type/*/@xmi.idref" />

  <tr>    
    <td class="t_cell">    
      <xsl:value-of select="Foundation.Core.ModelElement.name"/>
    </td>
    <td class="t_cell">
      <xsl:call-template name="parameters">
        <xsl:with-param name="ref" select="@xmi.id"/>
      </xsl:call-template>
      &#160;
    </td>
    <td class="t_cell">
      <xsl:if test="string-length($return_type) > 0">
        <xsl:call-template name="name_with_link">
          <xsl:with-param name="node" select="$return_type" />
        </xsl:call-template>
      </xsl:if>
      &#160;
    </td>
    <td class="t_cell">    
      <xsl:value-of select="Foundation.Core.ModelElement.visibility/@xmi.value"/>
    </td>
  </tr>
</xsl:template>



<xsl:template name="parameters">
  <xsl:param name="ref"/>
  <xsl:variable name="params" 
   select="Foundation.Core.BehavioralFeature.parameter/Foundation.Core.Parameter[(Foundation.Core.Parameter.kind/@xmi.value!='return') or not(Foundation.Core.Parameter.kind)]"/>

  <xsl:if test="count($params) > 0">
    <table cellpadding="2" cellspacing="1" border="0">
      <xsl:apply-templates select="$params"/>
    </table>
  </xsl:if>
</xsl:template>


<xsl:template match="Foundation.Core.Parameter">
  <tr>
    <td class="t_cell">
      <xsl:call-template name="name_with_link">
        <xsl:with-param name="node" select="Foundation.Core.Parameter.type/*/@xmi.idref"/>
      </xsl:call-template>
    </td>
    <td class="t_cell">
      <xsl:value-of select="Foundation.Core.ModelElement.name"/>
    </td>
  </tr>
</xsl:template>



<xsl:template name="name_with_link">
  <xsl:param name="node"/>
  <xsl:variable name="classifier" select="key('classifier', $node)" />
  <xsl:variable name="classifier_name" select="$classifier/Foundation.Core.ModelElement.name" />
  <xsl:variable name="p_id" select="$classifier/../@xmi.id" />
  <xsl:variable name="p_name" select="$classifier/../Foundation.Core.ModelElement.name" />
  <xsl:choose>
    <xsl:when test="$p_name = '((Unknown))'">
      <xsl:value-of select="$classifier_name"/>
    </xsl:when>
    <xsl:otherwise>
      <a href="p_{$p_id}.html#{$node}">
        <xsl:value-of select="$classifier_name"/>
      </a>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>







</xsl:stylesheet>


