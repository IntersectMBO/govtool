import * as React from 'react';
const CreateGA2 = (props) => (
    <svg
        xmlns='http://www.w3.org/2000/svg'
        width={995}
        height={867}
        fill='none'
        {...props}
    >
        <g filter='url(#a)'>
            <path
                fill='#D63F1E'
                fillOpacity={0.16}
                d='M189.856 92.112c154.29-154.29 196.05-284.502 505.972-221.109 154.83 154.83 115.926 490.77-38.364 645.06-154.29 154.29-566.466 215.759-721.296 60.929-154.83-154.83 99.398-330.59 253.688-484.88Z'
            />
        </g>
        <defs>
            <filter
                id='a'
                width={1306.4}
                height={1212.28}
                x={-312.045}
                y={-345.403}
                colorInterpolationFilters='sRGB'
                filterUnits='userSpaceOnUse'
            >
                <feFlood floodOpacity={0} result='BackgroundImageFix' />
                <feBlend
                    in='SourceGraphic'
                    in2='BackgroundImageFix'
                    result='shape'
                />
                <feGaussianBlur
                    result='effect1_foregroundBlur_966_121840'
                    stdDeviation={100}
                />
            </filter>
        </defs>
    </svg>
);
export default CreateGA2;
