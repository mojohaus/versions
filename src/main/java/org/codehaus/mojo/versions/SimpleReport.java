package org.codehaus.mojo.versions;

import org.apache.maven.doxia.sink.Sink;

import java.util.Locale;

/**
 * Created by IntelliJ IDEA.
 *
 * @author Stephen Connolly
 * @goal simple-report
 * @requiresDependencyResolution runtime
 * @requiresProject true
 * @since 31-Jan-2009 10:32:10
 */
public class SimpleReport
    extends AbstractVersionsReport
{
    public boolean isExternalReport()
    {
        return false;
    }

    public boolean canGenerateReport()
    {
        return true;
    }

    /**
     * generates an empty report in case there are no sources to generate a report with
     *
     * @param locale the locale to generate the report for.
     * @param sink   the report formatting tool
     */
    protected void doGenerateReport( Locale locale, Sink sink )
    {
        sink.head();
        sink.title();
        sink.text( getText( locale, "report.header" ) );
        sink.title_();
        sink.head_();

        sink.body();
        sink.section1();

        sink.sectionTitle1();
        sink.text( getText( locale, "report.mainTitle" ) );
        sink.sectionTitle1_();

        sink.paragraph();
        sink.text( getText( locale, "report.emptyDescription" ) );
        sink.paragraph_();

        sink.section1_();

        sink.body_();
        sink.flush();
        sink.close();
    }

    public String getOutputName()
    {
        return "simple-report";
    }
}
