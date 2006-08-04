<?xml version="1.0" encoding="iso-8859-1" ?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

 <xsl:param name="mode">xml</xsl:param>

 <xsl:output method="html"
             indent="yes"
             omit-xml-declaration="yes"
             doctype-public="-//W3C//DTD HTML 4.0 Strict//EN" />

 <xsl:template match="/document-part">

  <html
   xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>Lisp Machine Manual</title>
      <link rel="stylesheet" type="text/css" href="lmman.css" />
      <script src="javascript.js" language="javascript" type="text/javascript"> </script>
    </head>
  
    <body>
     <xsl:apply-templates/>
    </body>
  </html>
 </xsl:template>

 <!-- structural elements -->

 <xsl:template match="chapter">
  <h1><xsl:value-of select="number"/>. <xsl:value-of select="@title"/></h1>
  <xsl:apply-templates/>
 </xsl:template>

 <xsl:template match="section">
  <h2><xsl:value-of select="chapter-number"/>.<xsl:value-of select="number"/> <xsl:value-of select="@title"/></h2>
  <xsl:apply-templates/>
 </xsl:template>

 <xsl:template match="p">
  <p>
   <xsl:apply-templates/>
  </p>
 </xsl:template>

 <xsl:template match="pre">
<pre>
<xsl:apply-templates/>
</pre>
 </xsl:template>

 <!-- hyperlinks -->

 <xsl:template match="a">
  <a name="{@name}"/>
 </xsl:template>

 <xsl:template match="define">
  <a name="{@name}-{@type}"/>
  <div class="definition">
   <xsl:value-of select="@name"/>
   <xsl:apply-templates/>
  </div>
 </xsl:template>

 <xsl:template match="args">
  <div class="arguments">
   <xsl:apply-templates/>
  </div>
 </xsl:template>

 <xsl:template match="description">
  <xsl:apply-templates/>
 </xsl:template>

 <xsl:template match="ref">
  <a href="{@definition-in-file}.xml#{@key}"><xsl:value-of select="@title"/></a>
 </xsl:template>

 <!-- font selections -->

 <xsl:template match="obj">
  <span class="obj"><xsl:apply-templates/></span>
 </xsl:template>

 <xsl:template match="arg">
  <span class="arg"><xsl:apply-templates/></span>
 </xsl:template>

 <xsl:template match="lisp">
  <pre>
<xsl:apply-templates/></pre>
 </xsl:template>

 <!-- tables -->

 <xsl:template match="table">
  <table>
   <tbody>
    <xsl:apply-templates/>
   </tbody>
  </table>
 </xsl:template>

 <xsl:template match="tr">
  <tr>
   <xsl:apply-templates/>
  </tr>
 </xsl:template>

 <xsl:template match="td">
  <td>
   <xsl:apply-templates/>
  </td>
 </xsl:template>

</xsl:stylesheet>