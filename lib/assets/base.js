const BaseSelect = {
  name: "BaseSelect",

  props: {
    label: {
      type: String,
      default: "",
    },
    labelTooltip: {
      type: String,
      default: "",
    },
    selectClass: {
      type: String,
      default: "input",
    },
    modelValue: {
      type: String,
      default: "",
    },
    options: {
      type: Array,
      default: [],
      required: true,
    },
    required: {
      type: Boolean,
      default: false,
    },
    inline: {
      type: Boolean,
      default: false,
    },
    grow: {
      type: Boolean,
      default: false,
    },
  },

  methods: {
    available(value, options) {
      return value
        ? options.map((option) => option.value).includes(value)
        : true;
    },
    hasTooltip(labelTooltip) {
      return labelTooltip.length > 0;
    }
  },

  template: `
  <div v-bind:class="[inline ? 'inline-field' : 'field', grow ? 'grow' : '']">
    <label v-bind:class="inline ? 'inline-input-label' : 'input-label'">
      {{ label }}
    </label>
    <label v-bind:class="inline ? 'inline-input-label-tooltip' : 'input-label-tooltip'" v-if:"hasTooltip(labelTooltip)">
      {{ labelTooltip }}
    </label>
    <select
      :value="modelValue"
      v-bind="$attrs"
      @change="$emit('update:modelValue', $event.target.value)"
      v-bind:class="[selectClass, { unavailable: !available(modelValue, options) }]"
    >
      <option v-if="!required && !available(modelValue, options)"></option>
      <option
        v-for="option in options"
        :value="option.value"
        :key="option"
        :selected="option.value === modelValue"
      >{{ option.label }}</option>
      <option
        v-if="!available(modelValue, options)"
        class="unavailable"
        :value="modelValue"
      >{{ modelValue }}</option>
    </select>
  </div>
  `,
};

const BaseInput = {
  name: "BaseInput",
  props: {
    label: {
      type: String,
      default: "",
    },
    labelTooltip: {
      type: String,
      default: "",
    },
    inputClass: {
      type: String,
      default: "input",
    },
    modelValue: {
      type: [String, Number],
      default: "",
    },
    inline: {
      type: Boolean,
      default: false,
    },
    grow: {
      type: Boolean,
      default: false,
    },
    number: {
      type: Boolean,
      default: false,
    },
  },

  computed: {
    emptyClass() {
      if (this.modelValue === "") {
        return "empty";
      }
    },
  },

  methods: {
    hasTooltip(labelTooltip) {
      return labelTooltip.length > 0;
    }
  },

  template: `
  <div v-bind:class="[inline ? 'inline-field' : 'field', grow ? 'grow' : '']">
    <label v-bind:class="inline ? 'inline-input-label' : 'input-label'">
      {{ label }}
    </label>
    <label v-bind:class="inline ? 'inline-input-label-tooltip' : 'input-label-tooltip'" v-if:"hasTooltip(labelTooltip)">
      {{ labelTooltip }}
    </label>
    <input
      :value="modelValue"
      @input="$emit('update:modelValue', $event.target.value)"
      v-bind="$attrs"
      v-bind:class="inputClass"
    >
  </div>
  `,
};

const SmartCellConfig = (ctx, info, extra_components, extra_data) => {
  return {
    components: {
      BaseInput: BaseInput,
      BaseSelect: BaseSelect,
      ...extra_components
    },

    data() {
      return {
        fields: info.fields,
        ...extra_data
      };
    },

    methods: {
      handleFieldChange(event) {
        const { name: field, value } = event.target;

        if (field) {
          if (info.id == "evision.zoo") {
            ctx.pushEvent("update_field", { field, value });
          } else {
            const sub_value = field.split(".").reduce((data, key) => data[key], this.fields);
            ctx.pushEvent("update_field", { field, value: sub_value });
          }
        }
      },
    }
  }
};

export const Base = {
  BaseInput: BaseInput,
  BaseSelect: BaseSelect,
  SmartCellConfig: SmartCellConfig
};
