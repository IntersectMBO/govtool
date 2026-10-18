'use client';

import { ThemeProvider } from '@mui/material/styles';
import Loader from '../Loader';
import theme from './theme';

function ThemeProviderWrapper({ children }) {
    return (
        <ThemeProvider theme={theme}>
            <Loader />
            {children}
        </ThemeProvider>
    );
}

export default ThemeProviderWrapper;
