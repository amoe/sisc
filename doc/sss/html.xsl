<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'
                xmlns="http://www.w3.org/TR/xhtml1/transitional"
                exclude-result-prefixes="#default">

<xsl:import href="@@@doc.style@@@"/>

<xsl:variable name="chunk.section.depth">0</xsl:variable>
<xsl:variable name="html.stylesheet">sss.css</xsl:variable>
<!--
<xsl:variable name="default-classsynopsis-language">scheme</xsl:variable>
-->

<xsl:template match="classsynopsis
                     |fieldsynopsis
                     |methodsynopsis
                     |constructorsynopsis
                     |destructorsynopsis">
  <xsl:param name="language">
    <xsl:choose>
      <xsl:when test="@language">
	<xsl:value-of select="@language"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:value-of select="$default-classsynopsis-language"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>

  <xsl:choose>
    <xsl:when test="$language='java'">
      <xsl:apply-templates select="." mode="java"/>
    </xsl:when>
    <xsl:when test="$language='perl'">
      <xsl:apply-templates select="." mode="perl"/>
    </xsl:when>
    <xsl:when test="$language='idl'">
      <xsl:apply-templates select="." mode="idl"/>
    </xsl:when>
    <xsl:when test="$language='cpp'">
      <xsl:apply-templates select="." mode="cpp"/>
    </xsl:when>
    <xsl:when test="$language='scheme'">
      <xsl:apply-templates select="." mode="scheme"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:message>
	<xsl:text>Unrecognized language on </xsl:text>
        <xsl:value-of select="name(.)"/>
        <xsl:text>: </xsl:text>
	<xsl:value-of select="$language"/>
      </xsl:message>
      <xsl:apply-templates select=".">
	<xsl:with-param name="language"
	  select="$default-classsynopsis-language"/>
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="methodname" mode="scheme">
  <span class="{name(.)}">
    <xsl:apply-templates mode="scheme"/>
  </span>
</xsl:template>

<xsl:template match="methodparam" mode="scheme">
  <xsl:variable name="choice" select="@choice"/>
  <xsl:variable name="rep" select="@rep"/>
  <xsl:if test="position() &gt; 0">
    <xsl:text> </xsl:text>
  </xsl:if>
  <xsl:choose>
    <xsl:when test="$choice='opt'">
      <xsl:text>[</xsl:text>
      <xsl:if test="type">
        <xsl:text>(</xsl:text>
      </xsl:if>
      <xsl:if test="type">
        <span class="{name(.)}">
          <xsl:apply-templates select="type" mode="scheme"/>
        </span>
        <xsl:text> </xsl:text>
      </xsl:if>
      <span class="{name(.)}">
        <xsl:apply-templates select="parameter" mode="scheme"/>
      </span>
      <xsl:if test="type">
        <xsl:text>)</xsl:text>
      </xsl:if>
      <xsl:text>]</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="type">
        <xsl:text>(</xsl:text>
      </xsl:if>
      <xsl:if test="type">
        <span class="{name(.)}">
          <xsl:apply-templates select="type" mode="scheme"/>
        </span>
        <xsl:text> </xsl:text>
      </xsl:if>
      <span class="{name(.)}">
        <xsl:apply-templates select="parameter" mode="scheme"/>
      </span>
      <xsl:if test="type">
        <xsl:text>)</xsl:text>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:choose>
    <xsl:when test="$rep='repeat'">
      <xsl:text> ...</xsl:text>
    </xsl:when>
    <xsl:otherwise>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="type" mode="scheme">
  <span class="{name(.)}">
    <xsl:apply-templates mode="scheme"/>
  </span>
</xsl:template>

<xsl:template match="parameter" mode="scheme">
  <span class="{name(.)}">
    <xsl:apply-templates mode="scheme"/>
  </span>
</xsl:template>

<xsl:template match="methodsynopsis" mode="scheme">
  <code class="{name(.)}">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates select="methodname" mode="scheme"/>
    <xsl:apply-templates select="methodparam" mode="scheme"/>
    <xsl:text>)</xsl:text>
    <xsl:if test="type">
      <xsl:text> =&gt; </xsl:text> 
      <xsl:apply-templates select="type" mode="scheme"/>
    </xsl:if>
  </code>
<!--  <xsl:call-template name="synop-break"/>-->
</xsl:template>

</xsl:stylesheet>
