'use client';

import './index.css';
import { Box } from '@mui/material';
import ThemeProviderWrapper from './components/ThemeProviderWrapper';
import { ReadOnlyAppContextProvider } from './context/context';
import ProposedBudgetDiscussion from './pages/BudgetDiscussion';
import SingleBudgetDiscussion from './pages/BudgetDiscussion/SingleBudgetDiscussion';
import { ScrollToTop } from './lib/hooks';

// The read-only 2025 budget proposals archive. It reads static files only,
// so it needs no pdf API URL, wallet or forum session.
function BudgetArchiveApp({ view, id, category, showTitle, ...props }) {
    return (
        <div className='App' style={{ width: '100%', height: '100%' }}>
            <ReadOnlyAppContextProvider govtoolProps={props}>
                <ThemeProviderWrapper>
                    <Box
                        component='section'
                        display={'flex'}
                        flexDirection={'column'}
                        flexGrow={1}
                    >
                        <ScrollToTop />
                        {view === 'detail' ? (
                            <SingleBudgetDiscussion id={id} />
                        ) : (
                            <ProposedBudgetDiscussion
                                category={category}
                                showTitle={showTitle}
                            />
                        )}
                    </Box>
                </ThemeProviderWrapper>
            </ReadOnlyAppContextProvider>
        </div>
    );
}

export default BudgetArchiveApp;
