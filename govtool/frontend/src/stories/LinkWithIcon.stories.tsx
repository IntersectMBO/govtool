import { Meta, StoryObj } from '@storybook/react';
import { LinkWithIcon } from '@molecules';
import { ICONS } from '@consts';

const meta: Meta<typeof LinkWithIcon> = {
  title: 'Example/LinkWithIcon',
  component: LinkWithIcon,
  parameters: {
    layout: 'centered',
  },
};

export default meta;

export const Default: StoryObj<typeof LinkWithIcon> = {
  args: {
    label: 'Default Link',
  },
};

export const WithCustomIcon: StoryObj<typeof LinkWithIcon> = {
  args: {
    label: 'Custom Icon Link',
    icon: <img alt="custom icon" src={ICONS.link} />,
  },
};
