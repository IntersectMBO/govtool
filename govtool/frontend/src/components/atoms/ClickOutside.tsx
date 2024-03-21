import React, { useRef, useEffect, RefObject } from "react";

const useOutsideClick = (ref: RefObject<HTMLElement>, onClick: () => void) => {
  useEffect(() => {
    document.addEventListener("mousedown", (e) => {
      if (ref.current && !ref.current.contains(e.target as Node)) {
        onClick();
      }
    });

    return () => {
      document.removeEventListener("mousedown", (e) => {
        if (ref.current && !ref.current.contains(e.target as Node)) {
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
