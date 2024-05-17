import { BetaAnalyticsDataClient } from '@google-analytics/data';

const analyticsDataClient = new BetaAnalyticsDataClient({
    credentials: {
        client_email: process.env.GA_CLIENT_EMAIL,
        private_key: process.env.GA_PRIVATE_KEY.replace(/\\n/g, '\n')
    }
})

const propertyId = process.env.NEXT_PUBLIC_GA4_PROPERTY_ID;

export default async function handler(req, res) {
    try {
        const [response] = await analyticsDataClient.runReport({
            property: `properties/${propertyId}`,
            dateRanges: [{
                startDate: '2023-12-01',
                endDate: 'today',
            }],
            dimensions: [{ name: 'eventName' }],
            metrics: [{ name: 'eventCount' }],
            limit: 1000
        });

        const events = response.rows.map(row => ({
            eventName: row.dimensionValues[0].value,
            eventCount: row.metricValues[0].value
        }));

        res.status(200).json(events);
    } catch (error) {
        console.error('Error running report:', error);
        res.status(500).json({ message: 'Failed to fetch data' });
    }
}
