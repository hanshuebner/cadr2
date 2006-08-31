<?xml version="1.0" encoding="iso-8859-1" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                version="1.0" >

 <xsl:output indent="yes" />

 <xsl:template match="/document-root">

  <fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
   <fo:layout-master-set>
    <fo:simple-page-master master-name="A4"
                           page-height="29.7cm" 
                           page-width="21cm"
                           margin-top="1cm"
                           margin-bottom="2cm"
                           margin-left="2.5cm"
                           margin-right="2.5cm">
     <fo:region-body margin-top="3cm"/>
     <fo:region-before extent="3cm"/>
     <fo:region-after extent="3cm"/>
    </fo:simple-page-master>
   </fo:layout-master-set>
 
   <fo:page-sequence master-reference="A4">
    <fo:static-content flow-name="xsl-region-after">
     <fo:block text-align="end">Page <fo:page-number/></fo:block>
    </fo:static-content>
    <fo:flow flow-name="xsl-region-body">
     <xsl:apply-templates/>
    </fo:flow>
   </fo:page-sequence>
  </fo:root>
 </xsl:template>

 <xsl:template match="document-part">
  <xsl:apply-templates/>
 </xsl:template>

 <!-- including other files -->

 <xsl:template match="include">
  <xsl:apply-templates select="document(concat(@file, '.xml'))"/>
 </xsl:template>

 <!-- structural elements -->

 <xsl:template match="chapter">
  <fo:block font-family="sans-serif" font-size="25pt" break-before="page">
   <xsl:value-of select="@number"/>.<xsl:value-of select="' '"/><xsl:value-of select="@title"/>
   </fo:block>
  <xsl:apply-templates/>
 </xsl:template>

 <xsl:template match="section">
  <!-- <a name="{@name}-section"/> xxx --> 
  <fo:block id="{@name}-section" font-family="sans-serif" font-size="20pt" space-before="24pt" space-after="12pt">
   <xsl:value-of select="@chapter-number"/>.<xsl:value-of select="@number"/><xsl:value-of select="' '"/><xsl:value-of select="@title"/>
  </fo:block>
  <xsl:apply-templates/>
 </xsl:template>

 <xsl:template match="p">
  <fo:block text-align="start" font-family="serif" margin-left="5mm" space-after="12pt">
   <xsl:if test="@indent = '1'">
    <xsl:attribute text-indent="5mm">indented</xsl:attribute>
   </xsl:if>
   <xsl:apply-templates/>
  </fo:block>
 </xsl:template>

 <xsl:template match="group">
  <fo:block font-family="serif"> <!-- xxx -->
   <xsl:apply-templates/>
  </fo:block>
 </xsl:template>

 <xsl:template match="pre">
<fo:block font-family="monospace" white-space-collapse="false" wrap-option="no-wrap">
<xsl:apply-templates select="*|text()"/>
</fo:block>
 </xsl:template>

 <xsl:template match="center">
  <fo:block font-family="sans-serif" font-size="16pt"><xsl:apply-templates/></fo:block>
 </xsl:template>

 <!-- hyperlinks -->

 <xsl:template match="a">
 <!--
  <fo:block line-height="0pt" space-after="0pt" 
            font-size="0pt" id="{@name}"/> -->
  <!-- <a name="{@name}"/> xxx -->
 </xsl:template>

 <xsl:template match="index-entry">
<!--  <fo:block line-height="0pt" space-after="0pt" 
            font-size="0pt" id="{@title}"/> -->
  <!-- <a name="{@title}"/> xxx -->
 </xsl:template>

 <xsl:template match="definition">
  <fo:block space-before="12pt"> <!-- xxx -->
   <xsl:apply-templates/>
  </fo:block>
 </xsl:template>

 <xsl:template match="define">
  <fo:block id="{@key}"> <!-- xxx -->
   <fo:block>
    <!-- <a name="{@key}"/> -->
    <fo:inline font-weight="bold"><xsl:value-of select="@name"/></fo:inline>
    <xsl:value-of select="' '"/>
    <xsl:apply-templates/>
    <xsl:value-of select="' '"/>
    <fo:inline font-family="serif" font-style="italic" text-align="end">
     <xsl:choose>
      <xsl:when test="@type = 'message'">Message</xsl:when>
      <xsl:when test="@type = 'fun'">Function</xsl:when>
      <xsl:when test="@type = 'method'">Method</xsl:when>
      <xsl:when test="@type = 'metamethod'">Meta-Method</xsl:when>
      <xsl:when test="@type = 'const'">Constant</xsl:when>
      <xsl:when test="@type = 'condition'">Condition</xsl:when>
      <xsl:when test="@type = 'spec'">Special Form</xsl:when>
      <xsl:when test="@type = 'mac'">Macro</xsl:when>
      <xsl:when test="@type = 'flavor'">Flavor</xsl:when>
      <xsl:when test="@type = 'flavor-condition'">Flavor Condition</xsl:when>
      <xsl:when test="@type = 'condition-flavor'">Condition Flavor</xsl:when>
      <xsl:when test="@type = 'var'">Variable</xsl:when>
      <xsl:when test="@type = 'initoption'">Initialization Option</xsl:when>
      <xsl:when test="@type = 'meter'">Meter</xsl:when>
      <xsl:otherwise><xsl:value-of select="@type"/></xsl:otherwise>
     </xsl:choose>
    </fo:inline>
   </fo:block>
  </fo:block>
 </xsl:template>

 <xsl:template match="args">
  <fo:inline font-family="serif">
   <xsl:apply-templates/>
  </fo:inline>
 </xsl:template>

 <xsl:template match="description">
  <fo:block font-family="serif" margin-left="1cm" space-before="6pt">
   <xsl:apply-templates/>
  </fo:block>
 </xsl:template>

 <xsl:template match="ref">
  <xsl:if test="@key != ''">
   <fo:basic-link color="blue" internal-destination="@{key}">Page <fo:page-number-citation ref-id="{@key}"/></fo:basic-link>
  </xsl:if>
  <!-- <a href="{@definition-in-file}.xml#{@key}"><xsl:value-of select="@title"/></a> xxx -->
 </xsl:template>

 <xsl:template match="a">
  <!-- <a href="{@href}"><xsl:apply-templates/></a> xxx -->
 </xsl:template>

 <!-- font selections -->
 <xsl:template match="standard">
  <fo:inline font-family="serif"><xsl:apply-templates/></fo:inline>
 </xsl:template>

 <xsl:template match="obj">
  <fo:inline font-family="sans-serif" font-size="80%"><xsl:apply-templates/></fo:inline>
 </xsl:template>

 <xsl:template match="arg">
  <fo:inline font-style="italic"><xsl:apply-templates/></fo:inline>
 </xsl:template>

 <xsl:template match="lisp">
<fo:block font-family="monospace" white-space-collapse="false" wrap-option="no-wrap"><xsl:apply-templates select="*|text()"/></fo:block>
 </xsl:template>

 <!-- tables -->

 <xsl:template match="table">
  <!-- <table>
   <tbody> -->
   <fo:block>
    <xsl:apply-templates/>
   </fo:block>
    <!--
   </tbody>
  </table> -->
 </xsl:template>

 <xsl:template match="tr">
  <!-- <tr> -->
   <xsl:apply-templates/>
  <!-- </tr> -->
 </xsl:template>

 <xsl:template match="td">
  <!-- <td> -->
   <xsl:apply-templates/>
  <!-- </td> -->
 </xsl:template>

</xsl:stylesheet>