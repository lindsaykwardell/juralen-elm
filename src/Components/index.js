import { defineCustomElement } from 'vue';

import ZoomButtons from './ZoomButtons/ZoomButtons.ce.vue'

const ZoomButtonsComponent = defineCustomElement(ZoomButtons);

customElements.define('zoom-buttons', ZoomButtonsComponent);