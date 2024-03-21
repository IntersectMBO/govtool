import type { Meta, StoryFn } from '@storybook/react';

import { Field } from '@molecules';
import { ComponentProps } from 'react';

const meta: Meta<typeof Field.TextArea> = {
  title: 'Example/TextArea',
  component: Field.TextArea,
  parameters: {
    layout: 'centered',
  },
  tags: ['autodocs'],
};

export default meta;

const Template: StoryFn<ComponentProps<typeof Field.TextArea>> = (args) => (
  <Field.TextArea {...args} />
);

export const Default = Template.bind({});

export const WithLabel = Template.bind({});
WithLabel.args = {
  label: 'Label',
};

export const WithHelpfulText = Template.bind({});
WithHelpfulText.args = {
  helpfulText: 'Helpful text here',
};

export const Error = Template.bind({});
Error.args = {
  errorMessage: 'Error message',
};

export const ErrorAndLabel = Template.bind({});
ErrorAndLabel.args = {
  errorMessage: 'Error message',
  label: 'Label',
};

export const WithAllProps = Template.bind({});
WithAllProps.args = {
  label: 'Label',
  helpfulText: 'Helpful text',
  errorMessage: 'Error message',
};
