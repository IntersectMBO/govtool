import { useEffect, useState } from 'react';

import { useDebounce } from '../../lib/hooks';
import { InputBase } from '@mui/material';
import SearchIcon from '@mui/icons-material/Search';

// The pill search of GovTool's DataActionsBar.
const SearchInput = ({ onDebouncedChange, placeholder = 'Search...' }) => {
    const [inputValue, setInputValue] = useState('');
    const debouncedInputValue = useDebounce(inputValue);

    useEffect(() => {
        onDebouncedChange(debouncedInputValue);
    }, [debouncedInputValue, onDebouncedChange]);

    return (
        <InputBase
            fullWidth
            id='outlined-basic'
            placeholder={placeholder}
            value={inputValue || ''}
            onChange={(e) => setInputValue(e?.target?.value)}
            startAdornment={
                <SearchIcon
                    style={{
                        color: '#99ADDE',
                        height: 16,
                        marginRight: 4,
                        width: 16,
                    }}
                />
            }
            inputProps={{
                'data-testid': 'search-input',
            }}
            sx={{
                bgcolor: 'white',
                border: 1,
                borderColor: '#6E87D9',
                borderRadius: 50,
                boxShadow: '2px 2px 20px 0 rgba(0,0,0,0.05)',
                fontSize: { xxs: 16, sm: 11 },
                fontWeight: 500,
                height: 48,
                padding: '16px 24px',
                minWidth: 0,
                maxWidth: '100%',
            }}
        />
    );
};

export default SearchInput;
