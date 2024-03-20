import React, { useRef, useEffect } from 'react';

const useOutsideClick = (ref: any, onClick: () => void) => {
  useEffect(() => {
    document.addEventListener('mousedown', (e) => {
      if (ref.current && !ref.current.contains(e.target)) {
        onClick();
      }
    });

    return () => {
      document.removeEventListener('mousedown', (e) => {
        if (ref.current && !ref.current.contains(e.target)) {
          onClick();
        }
      });
    };
  }, [ref]);
};

interface Props {
  children: React.ReactElement;
  onClick: () => void;
}

export const ClickOutside = ({ children, onClick }: Props) => {
  const wrapperRef = useRef(null);
  useOutsideClick(wrapperRef, onClick);
  return <div ref={wrapperRef}>{children}</div>;
};
