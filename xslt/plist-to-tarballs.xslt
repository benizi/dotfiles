<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <!-- transform opensource.apple.com plist into list of tgz's -->
  <xsl:output method="text"/>
  <xsl:template match="/">
    <xsl:apply-templates select="/plist/dict/dict/key"/>
  </xsl:template>
  <xsl:template match="key">
    <xsl:text>https://opensource.apple.com/tarballs/</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>/</xsl:text>
    <xsl:value-of select="."/>
    <xsl:text>-</xsl:text>
    <xsl:value-of select="following-sibling::dict[1]/string"/>
    <xsl:text>.tar.gz&#10;</xsl:text>
  </xsl:template>
</xsl:stylesheet>
