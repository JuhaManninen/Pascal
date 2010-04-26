<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- Root -->
<xsl:template match="/">

<html>

  <link href="styles.css" rel="stylesheet" type="text/css"/>

  <head>
    <title>Package overview</title>
  </head>
  
  <body class="body">
    <h1 class="head">Package overview</h1>
    
    <a href="p_overview_diagram.html">Diagram</a>
    <br/> <br/>

    <table width="100%" cellpadding="3" cellspacing="2" border="1">
      <tr>
        <td class="ClassTableHeader">Package name</td>
        <td class="ClassTableHeader"></td>
      </tr>

      <xsl:apply-templates select="//Model_Management.Package">
        <xsl:sort select="Foundation.Core.ModelElement.name"/>
      </xsl:apply-templates>
    </table>

  </body>

</html>


</xsl:template>
<!-- End Root -->




<xsl:template match="Model_Management.Package">
  <xsl:variable name="pname" select="Foundation.Core.ModelElement.name"/>
  <xsl:if test="($pname!='((Unknown))') and ($pname!='')">
    <tr align="left">
      <td class="t_cell"> 
        <a href="p_{@xmi.id}.html"> <xsl:value-of select="$pname"/> </a>
      </td>
      <td class="t_cell"> 
        <a href="p_{@xmi.id}_diagram.html">Diagram</a> 
      </td>
    </tr>
  </xsl:if>
</xsl:template>





</xsl:stylesheet>


